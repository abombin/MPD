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

b<- q1[(q1$"DT"=="PHFNS" | q1$"DT"=="PHFS"),]
a<-as.vector(b$"Column 1")
w<- as.data.frame(b[, a])
loc <- cmdscale(w)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(b, loc),
       aes(y = MDS2, x = MDS1, colour = Treatment)) + 
  geom_point(size=4)+
  
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))
### save
ggsave("PHF_wunif_allz_t.png", dpi = 300, width=25, height=25, units= c("cm")) 


b<- q1[(q1$"DT"=="RHSNS" | q1$"DT"=="RHSS"),]
a<-as.vector(b$"Column 1")
w<- as.data.frame(b[, a])
loc <- cmdscale(w)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(b, loc),
       aes(y = MDS2, x = MDS1, colour = Treatment)) + 
  geom_point(size=4)+
  
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))
### save
ggsave("RHS_wunif_allz_t.png", dpi = 300, width=25, height=25, units= c("cm")) 