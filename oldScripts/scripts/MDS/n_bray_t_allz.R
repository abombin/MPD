library(dplyr)
library(vegan)
library(ggplot2)
library(ggpubr)

n_d_bray_z <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/n_d_bray_z.xlsx")

q<-n_d_bray_z
DT<-as.data.frame(paste(q$Diet, q$Treatment, sep=""))
q1<-cbind(DT, q)
colnames(q1)[1]<-"DT"

b<- q1[(q1$"Diet"=="R" ),]
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
ggsave("R_bray_allz_t.png", dpi = 300, width=25, height=25, units= c("cm")) 

### PR

b<- q1[(q1$"Diet"=="PR" ),]
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
ggsave("PR_bray_allz_t.png", dpi = 300, width=25, height=25, units= c("cm")) 

### PA

b<- q1[(q1$"Diet"=="PA" ),]
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
ggsave("PA_bray_allz_t.png", dpi = 300, width=25, height=25, units= c("cm")) 

