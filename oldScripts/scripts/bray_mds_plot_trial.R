library(vegan)
library(ggplot2)
library(dplyr)

q<- transpose_100_zotu_unmod
w<- select(q, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "D&T", Type))

bray.mds <- metaMDS(comm = w, distance = "bray", trace = FALSE, autotransform = FALSE)

MDS_xy <- data.frame(bray.mds$points)
MDS_xy$Diet <- q$Diet
ggplot(MDS_xy, aes(MDS1, MDS2,  color = Diet)) + geom_point() + theme_bw()

bray.mds$stress