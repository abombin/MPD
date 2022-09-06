library(readr)
library(readxl)
library(readr)
library(ggplot2)
library(vegan)
library(ggpubr)


all_d_zn_z <- read.delim("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_zn_z.txt")
q<-as.data.frame(all_d_zn_z)

q$Diet <- gsub("13", "11", q$Diet)
q$Diet <- gsub("PHS11", "PHS", q$Diet)
q$Diet <- factor(q$Diet, level=c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11"))
DT<-as.data.frame(paste(q$Diet, q$Treatment, sep=""))
q1<-cbind(DT, q)
colnames(q1)[1]<-"DT"
# R
a<- q1[(q1$"DT"=="RS" |q1$"DT"=="RHFS" | q1$"DT"=="RHSS"  ),]
b<- a[, -c(1:8) ]
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point(aes(shape=Diet, color=Diet, size=5))+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 44))+
  guides(size=FALSE)+
  guides(shape=guide_legend(override.aes=list(size=3)))
ggsave("alld_R_s_braymds_allz1.png", dpi = 300, width=25, height=25, units= c("cm"))

# P
a<- q1[(q1$"DT"=="PRS" |q1$"DT"=="PHFS" | q1$"DT"=="PHSS"  ),]
b<- a[, -c(1:8) ]
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point(aes(shape=Diet, color=Diet, size=5))+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 44))+
  guides(size=FALSE)+
  guides(shape=guide_legend(override.aes=list(size=3)))
ggsave("alld_P_s_braymds_allz1.png", dpi = 300, width=25, height=25, units= c("cm"))

# PA
a<- q1[(q1$"DT"=="PAS" |q1$"DT"=="PHFAS" | q1$"DT"=="PHSA6S"  ),]
b<- a[, -c(1:8) ]
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point(aes(shape=Diet, color=Diet, size=5))+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 44))+
  guides(size=FALSE)+
  guides(shape=guide_legend(override.aes=list(size=3)))
ggsave("alld_PA_s_braymds_allz1.png", dpi = 300, width=25, height=25, units= c("cm"))

