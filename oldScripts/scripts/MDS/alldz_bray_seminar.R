q<-as.data.frame(all_d_zn_z)

q$Diet <- gsub("13", "11", q$Diet)
q$Diet <- gsub("PHS11", "PHS", q$Diet)
DT<-as.data.frame(paste(q$Diet, q$Treatment, sep=""))
q1<-cbind(DT, q)
colnames(q1)[1]<-"DT"
q1$Diet <- factor(q1$Diet, levels = c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11"))
### R
a<- q1[(q1$"DT"=="RHFNS" |q1$"DT"=="RHSNS" | q1$"DT"=="RNS"  ),]
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
  geom_point(size=4)+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  ylim(0.2, -0.2)

ggsave("alld_R_ns_braymds_allz.png", dpi = 300, width=25, height=25, units= c("cm"))


a<- q1[(q1$"DT"=="RHFS" |q1$"DT"=="RHSS" | q1$"DT"=="RS"  ),]
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
  geom_point(size=4)+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 28))

ggsave("alld_R_s_braymds_allz_el.png", dpi = 300, width=25, height=25, units= c("cm"))

### peach

a<- q1[(q1$"DT"=="PHFNS" |q1$"DT"=="PHSNS" | q1$"DT"=="PRNS"  ),]
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
  geom_point(size=4)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))

ggsave("alld_P_ns_braymds_allz.png", dpi = 300, width=25, height=25, units= c("cm"))


a<- q1[(q1$"DT"=="PHFS" |q1$"DT"=="PHSS" | q1$"DT"=="PRS"  ),]
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
  geom_point(size=4)+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 28))

ggsave("alld_P_s_braymds_allz_el.png", dpi = 300, width=25, height=25, units= c("cm"))

### PA

a<- q1[(q1$"DT"=="PHFANS" |q1$"DT"=="PHSA6NS" | q1$"DT"=="PANS" | q1$"DT"=="PHSA11NS" ),]
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
  geom_point(size=4)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))

ggsave("alld_PA_ns_braymds_allz.png", dpi = 300, width=25, height=25, units= c("cm"))


a<- q1[(q1$"DT"=="PHFAS" |q1$"DT"=="PHSA6S" | q1$"DT"=="PAS" ),]
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
  geom_point(size=4)+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 28))

ggsave("alld_PA_s_braymds_allz_el.png", dpi = 300, width=25, height=25, units= c("cm"))