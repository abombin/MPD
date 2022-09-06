library(vegan)
library(dplyr)
a<-hfhs_100zotu_norm
b<- select(a, -c(Label))
c<-vegdist(b, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)
z<-as.matrix(c)
z1<-as.data.frame(z)

z1$Label = a$Label

e <- as.vector(a$Label) 


colnames(z1)<-e

write.csv(z1, "bray_curt_100_zotu_norm.csv")