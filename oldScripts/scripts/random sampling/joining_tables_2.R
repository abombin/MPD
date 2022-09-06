library(plyr)
library(Hmisc)
library(dplyr)
library(data.table)

w1<- HF_HS_samples_key_updated_for_sequences_results
i1<- n_hf_hs_trig_rand
i2<- drop_na(i1)
i2$DTGE<- as.character(i2$DTGE)

w1$newcolumn<- paste(w1$DT, w1$"Genetic line", sep="")
colnames(w1)[colnames(w1)=="newcolumn"] <- "DTG"

e1<- w1[(w1$"Type"=="HF"),]
e1$newcolumn<-"e1"
colnames(e1)[colnames(e1)=="newcolumn"] <- "Experiment"
e1$newcolumn<- paste(e1$DTG, e1$"Experiment")
colnames(e1)[colnames(e1)=="newcolumn"] <- "DTGE"

e2<- w1[(w1$"Type"=="HS"),]
e2$newcolumn<-"e2"
colnames(e2)[colnames(e2)=="newcolumn"] <- "Experiment"
e2$newcolumn<- paste(e2$DTG, e2$"Experiment")
colnames(e2)[colnames(e2)=="newcolumn"] <- "DTGE"

e3<- w1[(w1$"Type"=="N"),]
e3$newcolumn<-"e2"
colnames(e3)[colnames(e3)=="newcolumn"] <- "Experiment"
e3$newcolumn<- paste(e3$DTG, e3$"Experiment")
colnames(e3)[colnames(e3)=="newcolumn"] <- "DTGE"

w2<- rbind(e1,e2,e3)

i3<- setDT(i2)[DTGE %chin% w2$DTGE]

i4<-setDT( i3 )[ , counter := seq_len( .N ), by = DTGE ]
i4$newcolumn<- paste(i4$DTGE, i4$"counter")
colnames(i4)[colnames(i4)=="newcolumn"] <- "DTGEC"

w3<-setDT( w2 )[ , counter := seq_len( .N ), by = DTGE ]
w3$newcolumn<- paste(w3$DTGE, w3$"counter")
colnames(w3)[colnames(w3)=="newcolumn"] <- "DTGEC"

r1<-merge(w3, i4[, c("DTGEC", "trig")], by="DTGEC", all.x = TRUE)

colnames(r1)[colnames(r1)=="Genetic line"] <- "Genotype"
colnames(w3)[colnames(w3)=="Genetic line"] <- "Genotype"

o1<-r1 %>% count(Diet, Treatment, Genotype)
o1<- o1[order(o1$Treatment, o1$Genotype, o1$Diet),]

o2<-w3 %>% count(Diet, Treatment, Genotype)
o2<- o2[order(o2$Treatment, o2$Genotype, o2$Diet),]

o3<- i4 %>% count(Diet, Treatment, Genotype)
o3<- o3[order(o3$Treatment, o3$Genotype, o3$Diet),]

p1<- all.equal(o1,o2)

colnames(r1)[colnames(r1)=="Col1"] <- "Label"

t1<-hf_hs_phylum_norm

r2<- merge(t1, r1[, c("Label", "trig")], by="Label", all.x = TRUE)
