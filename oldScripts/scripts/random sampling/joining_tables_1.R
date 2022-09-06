
library(plyr)
library(Hmisc)
library(rstatix)
library(dplyr)
library(gtools)
library(data.table)


w1<- HF_HS_samples_key_updated_for_sequences_results
i1<- n_hf_hs_trig_rand
i2<- drop_na(i1)
w1$newcolumn<- paste(w1$DT, w1$"Genetic line", sep="")
colnames(w1)[colnames(w1)=="newcolumn"] <- "DTG"

e1<- w1[(w1$"Type"=="HF"),]
e1$newcolumn<-"e1"
colnames(e1)[colnames(e1)=="newcolumn"] <- "Experiment"
e1$newcolumn<- paste(e1$DTG, e1$"Experiment")
colnames(e1)[colnames(e1)=="newcolumn"] <- "DTGE"

i3<- setDT(i2)[DTGE %chin% e1$DTGE]


e1<- e1[order(e1$Treatment, e1$"Genetic line", e1$Diet),]
i3<- i3[order(i3$Treatment, i3$Genotype, i3$Diet),]



t1<-setDT( i3 )[ , counter := seq_len( .N ), by = DTGE ]
t1$newcolumn<- paste(t1$DTGE, t1$"counter")
colnames(t1)[colnames(t1)=="newcolumn"] <- "DTGEC"

t2<-setDT( e1 )[ , counter := seq_len( .N ), by = DTGE ]
t2$newcolumn<- paste(t2$DTGE, t2$"counter")
colnames(t2)[colnames(t2)=="newcolumn"] <- "DTGEC"

r3<-merge(t2, t1[, c("DTGEC", "trig")], by="DTGEC")
r3<- r3[order(r3$Treatment, r3$"Genetic line", r3$Diet),]

write.csv(r3, "hf_merged_trig.csv")
write.csv(i3, "hf_trig.csv")
write.csv(e1, "hf_seq_key.csv")

y1<-e1 %>% count(Diet, Treatment, "Genetic line")

y2<-r3 %>% count(Diet, Treatment, "Genetic line")

y3<- i3 %>% count(Diet, Treatment, "Genetic line")

r1<-merge(e1, i3[, c("DTGE", "trig")], by="DTGE")
r2<- r1[!duplicated(r1$trig),]





o1<- subset(e1, !(DTGE %in% i3$DTGE))

w1$newcolumn<- if (w1$Type=="HF") {w1$newcolumn<-"e1"} else {w1$newcolumn<-"e2"}


colnames(w1)[colnames(w1)=="newcolumn"] <- "Experiment"
w1$newcolumn<- paste(w1$DTG, w1$"Experiment")
colnames(w1)[colnames(w1)=="newcolumn"] <- "DTGE"
i2<- drop_na(i1)

w2<-merge(w1, i2[, c("DTGE", "trig")], by="DTGE")
w3<- w2[!duplicated(w2$trig),]


write.csv(w1, "hf_hs_phylum_norm.csv")

w4<-right_join(w1, i1, by = c("DTGE" = "DTGE"))
w5<- w4[!duplicated(w4$trig),]

w1$trig <- i1$trig[match(w1$DTGE, i1$DTGE)]

w6<- full_join(w1, i2, by = c("DTGE" = "DTGE"))
w7<-w6[!duplicated(w6$trig),]


i3<- setDT(i2)[DTGE %chin% w1$DTGE]


t1<- drop_na(i1)
t2<-as.data.frame(unique(t1$trig))
t3<- w2[!duplicated(w2$trig),]