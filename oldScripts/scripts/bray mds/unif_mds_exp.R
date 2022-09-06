library(ape)
library(dplyr)
library(vegan)
library(ggplot2)
library(ggpubr)


q5<-all_d_wunif_w_cat
DT<-as.data.frame(paste(q5$Diet, q5$Treatment, sep=""))
q1<-cbind(DT, q)
colnames(q1)[1]<-"DT"
# N vs HF
b<- q1[(q1$"DT"=="RNS" | q1$"DT"=="RHFNS"| q1$"DT"=="RHSNS"),]
a<-as.vector(b$X1)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
e2 <- as.vector(b$"Diet")
w1<-pcoa(w, correction="none", rn=NULL)
w2<-biplot(w1)


fit <- cmdscale(w,eig=TRUE, k=2) # k is the number of dim

x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = e2, cex=.7)


pdf(file="tmp.pdf",width=100,height=100) # if needed can save as pdf without showing graph in R
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
dev.off()

###
loc <- cmdscale(w)
x <- loc[, 1]
y <- -loc[, 2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = e2, cex=.7)


### final
q5<-all_d_wunif_w_cat
DT<-as.data.frame(paste(q5$Diet, q5$Treatment, sep=""))
q1<-cbind(DT, q)
colnames(q1)[1]<-"DT"
# N vs HF
b<- q1[(q1$"DT"=="RNS" | q1$"DT"=="RHFNS"| q1$"DT"=="RHSNS"),]
a<-as.vector(b$X1)
w<- as.data.frame(b[, a])
loc <- cmdscale(w)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(b, loc),
       aes(y = MDS2, x = MDS1, colour = DT)) + 
  geom_point()+
  stat_conf_ellipse(aes(color = DT, fill = DT), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = DT), geom = "polygon", alpha = 0.01, level=0.5)+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))
### save
ggsave("disc_alld_zn_r_ns_100.png", dpi = 100, width=25, height=25, units= c("cm"))
ggsave("disc_alld_zn_r_ns_300.png", dpi = 300, width=25, height=25, units= c("cm")) 


