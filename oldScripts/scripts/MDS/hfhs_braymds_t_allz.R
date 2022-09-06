q<-transpose_all_zotu_norm

q$Diet <- gsub("13", "11", q$Diet)
q$Diet <- gsub("PHS11", "PHS", q$Diet)
### HS
a<- q[(q$"DT"=="RHSNS" |q$"DT"=="RHSS"),]
b<- select(a, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Treatment)) + 
  geom_point(size=4)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))

ggsave("RHS_braymds_allz.png", dpi = 300, width=25, height=25, units= c("cm"))

a<- q[(q$"DT"=="PHS13NS" |q$"DT"=="PHS13S"),]
b<- select(a, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Treatment)) + 
  geom_point(size=4)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))

ggsave("PHS_braymds_allz.png", dpi = 300, width=25, height=25, units= c("cm"))

a<- q[(q$"DT"=="PHSA6NS" |q$"DT"=="PHSA6S"),]
b<- select(a, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)
loc <- cmdscale(z1)
MDS1 <- loc[, 1]
MDS2 <- -loc[, 2]
#plot
ggplot(cbind(a, loc),
       aes(y = MDS2, x = MDS1, colour = Treatment)) + 
  geom_point(size=4)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))

ggsave("PHSA6_braymds_allz.png", dpi = 300, width=25, height=25, units= c("cm"))