library(tidyr)
library(dplyr)
library(data.table)

q5<-all_tax_zn_5gen
colnames(q5)[9] <- "Species"
#All
q5a<- q5[!(q5$"taxonomy"=="B"| q5$taxonomy=="U"),] #for all levels but ZOTU
#phylum

y2<-q5a %>% group_by(id, Diet, Genotype, Treatment, Round, e, type, Phylum) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Phylum, value = Total, fill=0)
write.table(w1, "all_d_zn_p.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)

#class
y2<-q5a %>% group_by(id, Diet, Genotype, Treatment, Round, e, type, Class) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Class, value = Total, fill=0)
w1<-select(w1, -c(V1))
write.table(w1, "all_d_zn_c.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)

#order
y2<-q5a %>% group_by(id, Diet, Genotype, Treatment, Round, e, type, Order) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Order, value = Total, fill=0)
w1<-select(w1, -c(V1))
write.table(w1, "all_d_zn_o.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)

#family
y2<-q5a %>% group_by(id, Diet, Genotype, Treatment, Round, e, type, Family) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Family, value = Total, fill=0)
w1<-select(w1, -c(V1))
write.table(w1, "all_d_zn_f.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)

# genus
y2<-q5a %>% group_by(id, Diet, Genotype, Treatment, Round, e, type, Genus) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Genus, value = Total, fill=0)
w1<-select(w1, -c(V1))
write.table(w1, "all_d_zn_g.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)

#species
y2<-q5a %>% group_by(id, Diet, Genotype, Treatment, Round, e, type, Species) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Species, value = Total, fill=0)
w1<-select(w1, -c(V1))
write.table(w1, "all_d_zn_s.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)

#zotu
y2<-q5 %>% group_by(id, Diet, Genotype, Treatment, Round, e, type, zotu) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = zotu, value = Total, fill=0)
write.table(w1, "all_d_zn_z.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)

### 100 most abundant
#family 
e1<-aggregate(q5a$abundance, by=list(y=q5a$Family), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e5<-head(e3, 100)
r1<-setDT(q5a)[Family %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, e, type, Family) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Family, value = Total, fill=0)
write.table(w1, "all_d_zn100_f.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
#ZOTU
e1<-aggregate(q5$abundance, by=list(y=q5$zotu), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e5<-head(e3, 100)
r1<-setDT(q5)[zotu %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, e, type, zotu) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = zotu, value = Total, fill=0)
write.table(w1, "all_d_zn100_z.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)

### 10 most abundant
#phylum
e1<-aggregate(q5a$abundance, by=list(y=q5a$Phylum), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e5<-head(e3, 10)
r1<-setDT(q5a)[Phylum %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, type, Phylum) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Phylum, value = Total, fill=0)
write.table(w1, "all_d_zn10_p.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
# order
e1<-aggregate(q5a$abundance, by=list(y=q5a$Order), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e5<-head(e3, 10)
r1<-setDT(q5a)[Order %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, type, Order) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Order, value = Total, fill=0)
write.table(w1, "all_d_zn10_o.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
# class
e1<-aggregate(q5a$abundance, by=list(y=q5a$Class), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e5<-head(e3, 10)
r1<-setDT(q5a)[Class %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, type, Class) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Class, value = Total, fill=0)
write.table(w1, "all_d_zn10_c.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
# family
e1<-aggregate(q5a$abundance, by=list(y=q5a$Family), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e5<-head(e3, 10)
r1<-setDT(q5a)[Family %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, e, type, Family) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Family, value = Total, fill=0)
write.table(w1, "all_d_zn10_f.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
# genus
e1<-aggregate(q5a$abundance, by=list(y=q5a$Genus), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e3a<-e3[!(e3$"y"==" uncultured"| e3$"y"==" uncultured bacterium"),]
e5<-head(e3a, 10)
r1<-setDT(q5a)[Genus %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, type, Genus) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Genus, value = Total, fill=0)
write.table(w1, "all_d_zn10_g.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)

#species
e1<-aggregate(q5a$abundance, by=list(y=q5a$Species), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e3a<-e3[!(e3$"y"==" uncultured"| e3$"y"==" uncultured bacterium"| e3$y==" Ambiguous_taxa"|e3$y==" uncultured organism"| e3$y==" uncultured alpha proteobacterium"| e3$y==" metagenome"),]
e5<-head(e3a, 10)
r1<-setDT(q5a)[Species %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, type, Species) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Species, value = Total, fill=0)
write.table(w1, "all_d_zn10_s.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
# zotu
e1<-aggregate(q$abundance, by=list(y=q$zotu), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e5<-head(e3, 10)
r1<-setDT(q5a)[zotu %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, type, zotu) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = zotu, value = Total, fill=0)
write.table(w1, "hfhs_zn10_z.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)



###HFHS
q<-hfhs_zn_alltax
colnames(q)[1] <- "zotu"
colnames(q)[9] <- "id"
colnames(q)[10] <- "abundance"
colnames(q)[13] <- "Genotype"
colnames(q)[16] <- "type"
q5a<- q[!(q$"taxonomy"=="Unassigned"),] #for all levels but ZOTU

### 10 most abundant
# phylum
e1<-aggregate(q5a$abundance, by=list(y=q5a$Phylum), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e5<-head(e3, 10)
r1<-setDT(q5a)[Phylum %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, type, Phylum) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Phylum, value = Total, fill=0)
write.table(w1, "hfhs_zn10_p.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
# order
e1<-aggregate(q5a$abundance, by=list(y=q5a$Order), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e5<-head(e3, 10)
r1<-setDT(q5a)[Order %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, type, Order) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Order, value = Total, fill=0)
write.table(w1, "hfhs_zn10_o.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
# class
e1<-aggregate(q5a$abundance, by=list(y=q5a$Class), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e5<-head(e3, 10)
r1<-setDT(q5a)[Class %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, type, Class) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Class, value = Total, fill=0)
write.table(w1, "hfhs_zn10_c.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
# family
e1<-aggregate(q5a$abundance, by=list(y=q5a$Family), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e5<-head(e3, 10)
r1<-setDT(q5a)[Family %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, type, Family) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Family, value = Total, fill=0)
write.table(w1, "hfhs_zn10_f.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
# genus
e1<-aggregate(q5a$abundance, by=list(y=q5a$Genus), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e3a<-e3[!(e3$"y"=="uncultured"| e3$"y"=="uncultured bacterium"),]
e5<-head(e3a, 10)
r1<-setDT(q5a)[Genus %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, type, Genus) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Genus, value = Total, fill=0)
write.table(w1, "hfhs_zn10_g.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)

#species
e1<-aggregate(q5a$abundance, by=list(y=q5a$Species), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e3a<-e3[!(e3$"y"=="uncultured"| e3$"y"=="uncultured bacterium"| e3$y=="Ambiguous_taxa"|e3$y=="uncultured organism"| e3$y=="uncultured alpha proteobacterium"),]
e5<-head(e3a, 10)
r1<-setDT(q5a)[Species %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, type, Species) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = Species, value = Total, fill=0)
write.table(w1, "hfhs_zn10_s.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
# zotu
e1<-aggregate(q$abundance, by=list(y=q$zotu), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e5<-head(e3, 10)
r1<-setDT(q5a)[zotu %chin% e5$y]
y2<-r1 %>% group_by(id, Diet, Genotype, Treatment, Round, type, zotu) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
w1<- y2 %>% spread(key = zotu, value = Total, fill=0)
write.table(w1, "hfhs_zn10_z.txt", append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)