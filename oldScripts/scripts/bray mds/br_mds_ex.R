library(vegan)
library(ggplot2)
library(dplyr)
q<- w1
b<- q[(q$"DT"=="RHFNS" | q$"DT"=="RHFS" | q$"DT"=="PHFNS" | q$"DT"=="PHFS" | q$"DT"=="PHFANS" | q$"DT"=="PHFAS" ),]

w<- q[ -c(1:7) ]

bray.mds <- metaMDS(comm = w, distance = "bray", trace = FALSE, autotransform = FALSE)

MDS_xy <- data.frame(bray.mds$points)
MDS_xy$Diet <- q$Diet
MDS_xy$Treatment <- q$Treatment
ggplot(MDS_xy, aes(MDS1, MDS2,  color = Treatment, shape= Diet)) + geom_point(size=3) + theme_bw()

ggsave("bray_all_zotu_norm_hf.png", dpi = 300, width=20, height=20, units= c("cm"))


w1<-all_d_zn100_f
q<- w1
w<- q[ -c(1:7) ]
bray.mds <- metaMDS(comm = w, distance = "bray", trace = FALSE, autotransform = FALSE)
MDS_xy <- data.frame(bray.mds$points)
MDS_xy$Diet <- q$Diet
MDS_xy$Treatment <- q$Treatment
ggplot(MDS_xy, aes(MDS1, MDS2,  color = Diet, shape= Treatment)) + geom_point(size=3) + theme_bw()

ggsave("bray_all_zotu_norm_hf.png", dpi = 300, width=20, height=20, units= c("cm"))