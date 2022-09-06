setwd("C:/Users/abomb/Box/HF and HS/R/HF HS/output")
library(readr)
library(readxl)
library(readr)
library(ggplot2)
library(vegan)
library(ggpubr)
library(readr)
library(readxl)
library(readr)

q <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/transpose 100 zotu unmod_norm.xlsx") ### 100 zotu
q<-as.data.frame(q)

q<- subset(q, select = -c(DT) )

q$Diet <- gsub("13", "11", q$Diet)
q$Diet <- gsub("PHS11", "PHS", q$Diet)
q$Diet <- factor(q$Diet, level=c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11"))
DT<-as.data.frame(paste(q$Diet, q$Treatment, sep=""))
q1<-cbind(DT, q)
colnames(q1)[1]<-"DT"

# 
a<- q1[(q1$"DT"=="RHFNS" |q1$"DT"=="RHFS" | q1$"DT"=="RHSNS" | q1$"DT"=="RHSS" | q1$"DT"=="PHFNS" | q1$"DT"=="PHFS"
        | q1$"DT"=="PHSNS" | q1$"DT"=="PHSS" | q1$"DT"=="PHFANS" | q1$"DT"=="PHFAS" | q1$"DT"=="PHSA6NS" | q1$"DT"=="PHSA6S"
        | q1$"DT"=="PHSA11NS"),]
b<- a[, -c(1:10) ]
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point(aes(shape=Treatment, color=Diet, size=5))+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 44))+
  guides(size=FALSE)+
  guides(shape=guide_legend(override.aes=list(size=3)))+
  ylim(NA, 0.10)+
  xlim(-0.3, 0.3)
ggsave("hfhs_bray_100z_norm_alldt.png", dpi = 300, width=25, height=25, units= c("cm"))

### all zotu
q <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/transpose_all_zotu_norm.xlsx") ### all_zotu
q<-as.data.frame(q)
q<- subset(q, select = -c(DT) )

q$Diet <- gsub("13", "11", q$Diet)
q$Diet <- gsub("PHS11", "PHS", q$Diet)
q$Diet <- factor(q$Diet, level=c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11"))
DT<-as.data.frame(paste(q$Diet, q$Treatment, sep=""))
q1<-cbind(DT, q)
colnames(q1)[1]<-"DT"

# 
a<- q1[(q1$"DT"=="RHFNS" |q1$"DT"=="RHFS" | q1$"DT"=="RHSNS" | q1$"DT"=="RHSS" | q1$"DT"=="PHFNS" | q1$"DT"=="PHFS"
        | q1$"DT"=="PHSNS" | q1$"DT"=="PHSS" | q1$"DT"=="PHFANS" | q1$"DT"=="PHFAS" | q1$"DT"=="PHSA6NS" | q1$"DT"=="PHSA6S"
        | q1$"DT"=="PHSA11NS"),]
b<- a[, -c(1:10) ]
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point(aes(shape=Treatment, color=Diet, size=5))+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 44))+
  guides(size=FALSE)+
  guides(shape=guide_legend(override.aes=list(size=3)))
ggsave("hfhs_bray_allz_norm_alldt.png", dpi = 300, width=25, height=25, units= c("cm"))
