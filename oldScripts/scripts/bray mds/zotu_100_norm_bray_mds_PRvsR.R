library(dplyr)
library(vegan)
library(ggplot2)

q<- transpose_100_zotu_unmod_norm
b<- q[(q$"DT"=="RNS" | q$"DT"=="PRNS"),]
w<- select(b, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))

bray.mds <- metaMDS(comm = w, distance = "bray", trace = FALSE, autotransform = TRUE)

MDS_xy <- data.frame(bray.mds$points)
MDS_xy$Diet <- b$Diet
MDS_xy$Treatment <- b$Treatment
ggplot(MDS_xy, aes(MDS1, MDS2,  color = Treatment, shape= Diet)) + geom_point(size=3) + theme_bw()

ggsave("zotu_100_norm_bray_mds_PRvsR.png", dpi = 300, width=20, height=20, units= c("cm"))