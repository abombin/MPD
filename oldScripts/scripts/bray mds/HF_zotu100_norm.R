
library(vegan)
library(ggplot2)
library(dplyr)
q<- transpose_100_zotu_unmod_norm
b<- q[(q$"DT"=="RHFNS" | q$"DT"=="RHFS" | q$"DT"=="PHFNS" | q$"DT"=="PHFS" | q$"DT"=="PHFANS" | q$"DT"=="PHFAS" ),]
w<- select(b, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))

bray.mds <- metaMDS(comm = w, distance = "bray", trace = FALSE, autotransform = FALSE)

MDS_xy <- data.frame(bray.mds$points)
MDS_xy$DT <- b$DT
ggplot(MDS_xy, aes(MDS1, MDS2,  color = DT)) + geom_point() + theme_bw()
