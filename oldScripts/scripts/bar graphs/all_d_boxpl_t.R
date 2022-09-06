library(ggplot2)

# Total
all_d_total <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_dev.xlsx")
q3<-all_d_total
q3$Diet <- gsub("13", "11", q3$Diet)
q3$Diet <- gsub("PHS11", "PHS", q3$Diet)
colnames(q3)[6] <- "Total"
q3<-q3[!is.na(q3$Total), ]

q3$Diet <- factor(q3$Diet, levels = c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11")) # organize figure legend

p <- ggplot(q3, aes(x=Treatment, y=Total)) + 
  geom_boxplot()+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  ylim(0, 55)+
  scale_y_continuous(name="Total Larvae", breaks=c(0, 10,20,30,40,50), limits=(c(0,55)))
  
p+facet_grid(col=vars(Diet))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("alld_total_t.png", dpi = 300, width=30, height=15, units= c("cm"))

# Weight
all_d_weight <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_weight.xlsx")
q3<-all_d_weight
q3$Diet <- gsub("13", "11", q3$Diet)
q3$Diet <- gsub("PHS11", "PHS", q3$Diet)
colnames(q3)[4] <- "Weight"
q3<-q3[!is.na(q3$Weight), ]

q3$Diet <- factor(q3$Diet, levels = c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11")) # organize figure legend

p <- ggplot(q3, aes(x=Treatment, y=Weight)) + 
  geom_boxplot()+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  ylim(0, 2)+
  scale_y_continuous(name="Weight (mg)", breaks=c(0, 0.5,1), limits=(c(0,1)))

p+facet_grid(col=vars(Diet))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("alld_weight_t.png", dpi = 300, width=30, height=15, units= c("cm"))

# Development

all_d_dev <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_dev.xlsx")
q3<-all_d_dev
q3$Diet <- gsub("13", "11", q3$Diet)
q3$Diet <- gsub("PHS11", "PHS", q3$Diet)
colnames(q3)[7] <- "Days"
q3<-q3[!is.na(q3$Days), ]

q3$Diet <- factor(q3$Diet, levels = c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11")) # organize figure legend

p <- ggplot(q3, aes(x=Treatment, y=Days)) + 
  geom_boxplot()+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  scale_y_continuous(name="Days to Develop", breaks=c(0, 5,10,15,20), limits=(c(0,20)))
  

p+facet_grid(col=vars(Diet))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("alld_dev_t.png", dpi = 300, width=30, height=15, units= c("cm"))


# Trig

all_d_trig <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_trig.xlsx")
q3<-all_d_trig
q3$Diet <- gsub("13", "11", q3$Diet)
q3$Diet <- gsub("PHS11", "PHS", q3$Diet)
q3$trig<-(((q3$"Mean(Triglyceride)"*2.5*0.005)/q3$Larvae)/q3$"Mean(weight)")
q3<-q3[!is.na(q3$trig), ]

q3$Diet <- factor(q3$Diet, levels = c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11")) # organize figure legend

p <- ggplot(q3, aes(x=Treatment, y=trig)) + 
  geom_boxplot()+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  scale_y_continuous(name="Triglyceride (mg)", breaks=c(0, 0.01,0.02,0.03,0.04), limits=(c(0,0.042)))
  
p+facet_grid(col=vars(Diet))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("alld_trig_t.png", dpi = 300, width=30, height=15, units= c("cm"))

# Gluc

all_d_gluc <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_gluc.xlsx")
q3<-all_d_gluc
q3$Diet <- gsub("13", "11", q3$Diet)
q3$Diet <- gsub("PHS11", "PHS", q3$Diet)
q3$gluc<-(((q3$"Mean(Glucose)"*0.035)/q3$Larvae)/q3$"Mean(weight)")
q3<-q3[!is.na(q3$gluc), ]

q3$Diet <- factor(q3$Diet, levels = c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11")) # organize figure legend

p <- ggplot(q3, aes(x=Treatment, y=gluc)) + 
  geom_boxplot()+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  scale_y_continuous(name="Glucose (mg)", breaks=c(0, 0.001,0.002,0.003), limits=(c(0,0.0035)))

p+facet_grid(col=vars(Diet))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("alld_gluc_t.png", dpi = 300, width=30.5, height=15, units= c("cm"))

