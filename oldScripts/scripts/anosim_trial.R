library(dplyr)
library(vegan)

q<- transpose_100_zotu_unmod_norm
w<- select(q, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))


e1 <- as.vector(q$"DT") 


r<- anosim(w, e1, permutations = 999, distance = "bray", strata = NULL)

t<-as.data.frame(r$signif)
t$new.col<-c("R")

t1<-as.data.frame(r$signif)
t1$new.col<-c("PR")

y<-rbind(t,t1)
