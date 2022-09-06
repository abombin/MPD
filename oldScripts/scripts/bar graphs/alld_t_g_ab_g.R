library(ggplot2)
library(dplyr)
library(tidyr)
# Import reed
setwd("C:/Users/reedlab/Box/HF and HS/R/HF HS/output")

# import
tr_all_d_zn10_g <- read_excel("C:/Users/reedlab/Box/HF and HS/R/HF HS/input/tr_all_d_zn10_g.xlsx", 
                              col_types = c("text", "text", "text", 
                                            "text", "numeric", "text", "text", 
                                            "numeric"))
group.colors <- c("#000000",  "#999999",  "#0072B2", "#009E73", "#D55E00","#F0E442", "#CC79A7", "#E69F00","#56B4E9","green" ) # make color panel

q3<-tr_all_d_zn10_g
q3$Diet <- gsub("13", "11", q3$Diet)
q3$Diet <- gsub("PHS11", "PHS", q3$Diet)

p <- ggplot(q3, aes(x=Genotype, y=abundance, fill=Label)) + 
  geom_bar(position="fill", stat="identity")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  scale_fill_manual(values = group.colors)+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  
  labs(fill = "Taxa", y="Abundance")
  
  
 
p+facet_grid(col=vars(Diet), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("alld_t_g_ab_g.png", dpi = 300, width=80, height=40, units= c("cm"))
  
