# This code performs a decomposition analysis of the increase in the number of 
# cataract cases in the Chinese population from 1990 to 2021. 
# By changing the relevant parameters, decomposition analysis can be conducted 
# for different genders.

library(dplyr)
library(data.table)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(writexl)
library(readxl)

#Calculate the age composition of the total population in 1990
pop1<-subset(population,population$year==1990 & population$sex_name == "Both")
tot1990<-sum(pop1$val)
pop1$percent <- pop1$val/tot1990

#Calculate the age composition of the total population in 2021
pop2<-subset(population,population$year==2021 & population$sex_name == "Both")
tot2021<-sum(pop2$val)
pop2$percent <- pop2$val/tot2021

### "a" represents the age proportion, and "p" represents the total population
a_1990 <- pop1$percent
a_2021 <- pop2$percent
print(a_2021)
p_1990 <- tot1990
p_2021 <- tot2021


case_1990 <- cataract_data %>% filter(year == 1990 &
                                 sex_name == "Both" &
                                 measure_name == 'Prevalence' )

case_2021 <- cataract_data %>% filter(year == 2021 &
                                 sex_name == "Both" &
                                 measure_name == 'Prevalence' )

age_order <- c("<5", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
               "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
               "70-74", "75-79", "80-84", "85-89", "90-94", "95+")
case_1990$age_name <- factor(case_1990$age_name, levels = age_order, ordered = TRUE)
case_1990 <- case_1990[order(case_1990$age_name), ]
case_2021$age_name <- factor(case_2021$age_name, levels = age_order, ordered = TRUE)
case_2021 <- case_2021[order(case_2021$age_name), ]


r_1990 <- as.numeric(case_1990$val)/10^5 
r_2021 <- as.numeric(case_2021$val)/10^5 
##### Calculate the proportion ####

a_effect <- round((sum(a_2021*p_1990*r_1990) + sum(a_2021*p_2021*r_2021))/3 + 
                    (sum(a_2021*p_1990*r_2021) + sum(a_2021*p_2021*r_1990))/6 -
                    (sum(a_1990*p_1990*r_1990) + sum(a_1990*p_2021*r_2021))/3 -
                    (sum(a_1990*p_1990*r_2021) + sum(a_1990*p_2021*r_1990))/6,3)

p_effect <- round((sum(a_1990*p_2021*r_1990) + sum(a_2021*p_2021*r_2021))/3 + 
                    (sum(a_1990*p_2021*r_2021) + sum(a_2021*p_2021*r_1990))/6 -
                    (sum(a_1990*p_1990*r_1990) + sum(a_2021*p_1990*r_2021))/3 -
                    (sum(a_1990*p_1990*r_2021) + sum(a_2021*p_1990*r_1990))/6,3)

r_effect <- round((sum(a_1990*p_1990*r_2021) + sum(a_2021*p_2021*r_2021))/3 + 
                    (sum(a_1990*p_2021*r_2021) + sum(a_2021*p_1990*r_2021))/6 -
                    (sum(a_1990*p_1990*r_1990) + sum(a_2021*p_2021*r_1990))/3 -
                    (sum(a_1990*p_2021*r_1990) + sum(a_2021*p_1990*r_1990))/6,3)

overll_differ <- round(a_effect + p_effect + r_effect,2)

a_percent <- round(a_effect/overll_differ*100,2)
p_percent <- round(p_effect/overll_differ*100,2)
r_percent <- round(r_effect/overll_differ*100,2)

temp <- c("Both",overll_differ,a_effect,p_effect,r_effect,a_percent,
          p_percent,r_percent) %>% t() %>% as.data.frame()

# Generate decomposition analysis result data
decomposition_name<-c('sex_name','overll_difference','a_effect','p_effect','r_effect','a_percent',
                      'p_percent','r_percent')
names(temp) <- decomposition_name
decomposition_data<-temp

write_xlsx(cataract_data, "C:/Users/30223/Desktop/cataract_data.xlsx")
write_xlsx(population, "C:/Users/30223/Desktop/population.xlsx")

