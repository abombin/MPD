library(dplyr)
library(vegan)
library(ggplot2)
library(ggpubr)
q<-transpose_100_zotu_unmod_norm
q$Diet <- gsub("13", "11", q$Diet)
q$Diet <- gsub("PHS11", "PHS", q$Diet)
#HFNS
a<- q[(q$"DT"=="RHFNS" |q$"DT"=="PHFNS" | q$"DT"=="PHFANS"  ),]
b<- select(a, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point()+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))

ggsave("hf_ns_braymds_100z.png", dpi = 300, width=20, height=20, units= c("cm"))

### HF S
a<- q[(q$"DT"=="RHFS" |q$"DT"=="PHFS" | q$"DT"=="PHFAS"  ),]
b<- select(a, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point()+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))

ggsave("hf_s_braymds_100z.png", dpi = 300, width=20, height=20, units= c("cm"))

### HS NS

a<- q[(q$"DT"=="RHSNS" |q$"DT"=="PHS13NS" | q$"DT"=="PHSA13NS" | q$"DT"=="PHSA6NS"  ),]
b<- select(a, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point()+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))

ggsave("hs_ns_braymds_100z.png", dpi = 300, width=20, height=20, units= c("cm"))


### HS NS

a<- q[(q$"DT"=="RHSS" |q$"DT"=="PHS13S" | q$"DT"=="PHSA6S"  ),]
b<- select(a, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point()+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))

ggsave("hs_s_braymds_100z.png", dpi = 300, width=20, height=20, units= c("cm"))




### no elipses
#HFNS

q<-transpose_100_zotu_unmod_norm
q$Diet <- gsub("13", "11", q$Diet)
q$Diet <- gsub("PHS11", "PHS", q$Diet)
a<- q[(q$"DT"=="RHFNS" |q$"DT"=="PHFNS" | q$"DT"=="PHFANS"  ),]
b<- select(a, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point(size=4)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))

ggsave("hf_ns_braymds_100z.png", dpi = 300, width=25, height=25, units= c("cm"))

### HF S
a<- q[(q$"DT"=="RHFS" |q$"DT"=="PHFS" | q$"DT"=="PHFAS"  ),]
b<- select(a, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point(size=4)+
  
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))

ggsave("hf_s_braymds_100z.png", dpi = 300, width=25, height=25, units= c("cm"))

### HS NS

a<- q[(q$"DT"=="RHSNS" |q$"DT"=="PHS13NS" | q$"DT"=="PHSA13NS" | q$"DT"=="PHSA6NS"  ),]
b<- select(a, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point(size=4)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))

ggsave("hs_ns_braymds_100z.png", dpi = 300, width=25, height=25, units= c("cm"))


### HS S

a<- q[(q$"DT"=="RHSS" |q$"DT"=="PHS13S" | q$"DT"=="PHSA6S"| q$"DT"=="PHSA13S"  ),]
b<- select(a, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Diet)) + 
  geom_point(size=4)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))

ggsave("hs_s_braymds_100z.png", dpi = 300, width=25, height=25, units= c("cm"))
