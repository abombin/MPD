library(dplyr)
library(vegan)
library(ggplot2)
library(ggpubr)

q5<-all_d_wunif_5gen
q5$Diet <- gsub("13", "11", q5$Diet)
q5$Diet <- gsub("PHS11", "PHS", q5$Diet)
DT<-as.data.frame(paste(q5$Diet, q5$Treatment, sep=""))
q1<-cbind(DT, q5)
colnames(q1)[1]<-"DT"
#  RS
b<- q1[(q1$"DT"=="RHFS" | q1$"DT"=="RHSS"| q1$"DT"=="RS"),]
a<-as.vector(b$"X1")
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
ggsave("alld_RS_wunif_allz.png", dpi = 300, width=25, height=25, units= c("cm")) 

b<- q1[(q1$"DT"=="PHFS" | q1$"DT"=="PHSS"| q1$"DT"=="PRS"),]
a<-as.vector(b$"X1")
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
ggsave("alld_PS_wunif_allz.png", dpi = 300, width=25, height=25, units= c("cm")) 


b<- q1[(q1$"DT"=="PHFAS" | q1$"DT"=="PHSA6S"| q1$"DT"=="PAS"| q1$"DT"=="PHSA11S"),]
a<-as.vector(b$"X1")
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
ggsave("alld_PAS_wunif_allz.png", dpi = 300, width=25, height=25, units= c("cm")) 


b<- q1[(q1$"DT"=="PHFANS" | q1$"DT"=="PHSA6NS"| q1$"DT"=="PANS"| q1$"DT"=="PHSA11NS"),]
a<-as.vector(b$"X1")
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
ggsave("alld_PANS_wunif_allz.png", dpi = 300, width=25, height=25, units= c("cm")) 