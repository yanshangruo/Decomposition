library(ggsci)
library(dplyr)
library(data.table)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(writexl)
library(readxl)
library(showtext)
catade <- read_excel("C:/Users/30223/Desktop/GBDcata/decomposition/tables.xlsx")

catade$value<-as.numeric(catade$value)
catade$overll<-as.numeric(catade$overll)

custom_legend_labels <- c(
  "Aging",
  "Population",
  "Epidemiological change"
)
# vidc<-vidc%>%mutate(value2=value/1000000)
# vidc<-vidc%>%mutate(Overll2=Overll/1000000)
catade$cata <- factor(catade$cata, levels = c("a", "p", "e"))
catade$py<-factor(catade$py,levels=c("pre","yld"),
                     labels=c("Prevalence","YLDs"))
ggplot() +
  # geom_hline(yintercept = c(0,5,10,15,20,25),color="#d9d9d9",alpha=0.5)+
  geom_bar(data = catade, 
           mapping = aes(x = sex, y = value, fill = cata), 
           stat = "identity", 
           position = "stack", 
           width = 0.7, 
           alpha = 0.9) +
  facet_grid(py~.) +
  coord_flip() +
  geom_point(data = catade,mapping = aes(x=sex,y=overll),size=2.0)+
  theme_classic()+
  scale_x_discrete(labels = c( "Both","Female","Male")) + 
  scale_y_continuous(breaks = seq(0,1.5e+07, by = 5e+6))+
  scale_fill_jama(labels = custom_legend_labels)+
  labs(x = "Sex",y = "Number")+
  theme(legend.position = c(0.78,0.2),              
        legend.title = element_blank(),  
        legend.box = "vertical",
        text = element_text(size = 10))             
ggsave(
  filename = "plot.tiff", 
  plot = last_plot(),                
  width = 6,                        
  height = 4,                         
  dpi = 600)

