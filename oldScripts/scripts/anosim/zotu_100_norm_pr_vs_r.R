library(dplyr)
library(vegan)

q<- transpose_100_zotu_unmod_norm
b<- q[(q$"DT"=="RNS" | q$"DT"=="PRNS"),]
w<- select(b, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))


e1 <- as.vector(b$"DT") 


r<- anosim(w, e1, permutations = 999, distance = "bray", strata = NULL)

t<-as.data.frame(r$signif)
t$new.col<-c("PRNS_VS_RNS")

