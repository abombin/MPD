library(dplyr)
library(vegan)
library(ggplot2)
library(ggpubr)

q5<-hfhs_wunif_all
q5$Diet <- gsub("13", "11", q5$Diet)
q5$Diet <- gsub("PHS11", "PHS", q5$Diet)
DT<-as.data.frame(paste(q5$Diet, q5$Treatment, sep=""))
q1<-cbind(DT, q5)
colnames(q1)[1]<-"DT"
#  HF NS
b<- q1[(q1$"DT"=="RHFNS" | q1$"DT"=="PHFNS"| q1$"DT"=="PHFANS"),]
a<-as.vector(b$"Column 1")
w<- as.data.frame(b[, a])
loc <- cmdscale(w)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(b, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point()+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))
### save
ggsave("disc_alld_zn_r_ns_300.png", dpi = 300, width=25, height=25, units= c("cm")) 

#  HF NS no elipses
b<- q1[(q1$"DT"=="RHFNS" | q1$"DT"=="PHFNS"| q1$"DT"=="PHFANS"),]
a<-as.vector(b$"Column 1")
w<- as.data.frame(b[, a])
loc <- cmdscale(w)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(b, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point(size=4)+
  

  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))
### save
ggsave("hf_ns_wunif_all_d.png", dpi = 300, width=25, height=25, units= c("cm")) 

#  HF S
b<- q1[(q1$"DT"=="RHFS" | q1$"DT"=="PHFS"| q1$"DT"=="PHFAS"),]
a<-as.vector(b$"Column 1")
w<- as.data.frame(b[, a])
loc <- cmdscale(w)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(b, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point(size=4)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))
### save
ggsave("hf_s_wunif_all_d.png", dpi = 300, width=25, height=25, units= c("cm"))

### HS NS
b<- q1[(q1$"DT"=="RHSNS" | q1$"DT"=="PHSNS"| q1$"DT"=="PHSA6NS"| q1$"DT"=="PHSA11NS"),]
a<-as.vector(b$"Column 1")
w<- as.data.frame(b[, a])
loc <- cmdscale(w)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(b, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point(size=4)+
  
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))
### save
ggsave("hs_ns_wunif_all_d.png", dpi = 300, width=25, height=25, units= c("cm")) 

### HS S

b<- q1[(q1$"DT"=="RHSS" | q1$"DT"=="PHSS"| q1$"DT"=="PHSA6S"| q1$"DT"=="PHSA11S"),]
a<-as.vector(b$"Column 1")
w<- as.data.frame(b[, a])
loc <- cmdscale(w)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(b, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point(size=4)+
  
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))
### save
ggsave("hs_s_wunif_all_d.png", dpi = 300, width=25, height=25, units= c("cm")) 