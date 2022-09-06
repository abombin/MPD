
library(vegan)
library(ggplot2)
library(dplyr)
q<- transpose_100_zotu_unmod_norm
b<- q[(q$"DT"=="RHFNS" | q$"DT"=="RHFS" | q$"DT"=="PHFNS" | q$"DT"=="PHFS" | q$"DT"=="PHFANS" | q$"DT"=="PHFAS" ),]
w<- select(b, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))

bray.mds <- metaMDS(comm = w, distance = "bray", trace = FALSE, autotransform = FALSE)

MDS_xy <- data.frame(bray.mds$points)
MDS_xy$Diet <- b$Diet
MDS_xy$Treatment <- b$Treatment
ggplot(MDS_xy, aes(MDS1, MDS2,  color = Treatment, shape= Diet)) + geom_point(size=3) + theme_bw()

ggsave("bray.png", dpi = 300, width=20, height=20, units= c("cm"))

# plot options: ggsave(plot = last_plot(), device = "jpeg",path = NULL, scale = 1, width = NA, 
# height = NA, units = c("in", "cm", "mm"),dpi = 300, limitsize = TRUE)


