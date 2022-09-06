library(vegan)
library(ggplot2)
library(dplyr)
q<- transpose_all_zotu_norm
b<- q[(q$"DT"=="RHFNS" |q$"DT"=="PHFNS" | q$"DT"=="PHFANS"  ),]
w<- select(b, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))

bray.mds <- metaMDS(comm = w, distance = "bray", trace = FALSE, autotransform = FALSE)

MDS_xy <- data.frame(bray.mds$points)
MDS_xy$Diet <- b$Diet
MDS_xy$Treatment <- b$Treatment
ggplot(MDS_xy, aes(MDS1, MDS2,  color = Diet)) + 
  geom_point()+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))



ggsave("bray_all_zotu_norm_hf.png", dpi = 300, width=20, height=20, units= c("cm"))