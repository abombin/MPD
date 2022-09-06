all_d_total <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_total.xlsx", 
                          col_types = c("numeric", "text", "text", 
                                        "text", "numeric", "numeric", "numeric", 
                                        "text", "numeric", "numeric"))
q3<-all_d_total
q3$Diet <- gsub("13", "11", q3$Diet)
q3$Diet <- gsub("PHS11", "PHS", q3$Diet)
colnames(q3)[6] <- "Total"
colnames(q3)[3] <- "Genotype"
q3<-q3[!is.na(q3$Total), ]

q3$Diet <- factor(q3$Diet, levels = c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11")) # organize figure legend
q4<-q3[(q3$"Genotype"=="153" |q3$"Genotype"=="748" |q3$"Genotype"=="787" |q3$"Genotype"=="802" |q3$"Genotype"=="805"),]
p <- ggplot(q4, aes(x=Genotype, y=Total)) + 
  geom_boxplot()+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  ylim(0, 55)+
  scale_y_continuous(name="Total Larvae", breaks=c(0, 10,20,30,40,50), limits=(c(0,50)))

p+facet_grid(col=vars(Diet), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("alld_t_g_total.png", dpi = 300, width=60, height=30, units= c("cm"))
