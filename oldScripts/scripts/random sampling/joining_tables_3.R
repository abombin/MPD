library(plyr)
library(Hmisc)
library(dplyr)
library(data.table)

w1<- HF_HS_samples_key_updated_for_sequences_results
# trig
i1<- n_hf_hs_trig_rand
i2<- drop_na(i1)
i2$DTGE<- as.character(i2$DTGE)
# base
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
# Trig
i3<- setDT(i2)[DTGE %chin% w2$DTGE]

i4<-setDT( i3 )[ , counter := seq_len( .N ), by = DTGE ]
i4$newcolumn<- paste(i4$DTGE, i4$"counter")
colnames(i4)[colnames(i4)=="newcolumn"] <- "DTGEC"
# Base
w3<-setDT( w2 )[ , counter := seq_len( .N ), by = DTGE ]
w3$newcolumn<- paste(w3$DTGE, w3$"counter")
colnames(w3)[colnames(w3)=="newcolumn"] <- "DTGEC"
# Merge
r1<-merge(w3, i4[, c("DTGEC", "trig")], by="DTGEC", all.x = TRUE)

# Gluc
i1<- n_hf_hs_gluc_rand
i2<- drop_na(i1)
i2$DTGE<- as.character(i2$DTGE)

i3<- setDT(i2)[DTGE %chin% w2$DTGE]

i4<-setDT( i3 )[ , counter := seq_len( .N ), by = DTGE ]
i4$newcolumn<- paste(i4$DTGE, i4$"counter")
colnames(i4)[colnames(i4)=="newcolumn"] <- "DTGEC"

r2<-merge(r1, i4[, c("DTGEC", "gluc")], by="DTGEC", all.x = TRUE)

#Prot
i1<- n_hf_hs_prot_rand
i2<- drop_na(i1)
i2$DTGE<- as.character(i2$DTGE)

i3<- setDT(i2)[DTGE %chin% w2$DTGE]

i4<-setDT( i3 )[ , counter := seq_len( .N ), by = DTGE ]
i4$newcolumn<- paste(i4$DTGE, i4$"counter")
colnames(i4)[colnames(i4)=="newcolumn"] <- "DTGEC"

r3<-merge(r2, i4[, c("DTGEC", "prot")], by="DTGEC", all.x = TRUE)

#Weight
i1<- n_hf_hs_weight_rand
i2<- drop_na(i1)
i2$DTGE<- as.character(i2$DTGE)

i3<- setDT(i2)[DTGE %chin% w2$DTGE]

i4<-setDT( i3 )[ , counter := seq_len( .N ), by = DTGE ]
i4$newcolumn<- paste(i4$DTGE, i4$"counter")
colnames(i4)[colnames(i4)=="newcolumn"] <- "DTGEC"

r4<-merge(r3, i4[, c("DTGEC", "weight")], by="DTGEC", all.x = TRUE)

#development
i1<- n_hf_hs_dev_rand
i2<- drop_na(i1)
i2$DTGE<- as.character(i2$DTGE)

i3<- setDT(i2)[DTGE %chin% w2$DTGE]

i4<-setDT( i3 )[ , counter := seq_len( .N ), by = DTGE ]
i4$newcolumn<- paste(i4$DTGE, i4$"counter")
colnames(i4)[colnames(i4)=="newcolumn"] <- "DTGEC"

r5<-merge(r4, i4[, c("DTGEC", "dev")], by="DTGEC", all.x = TRUE)

#total
i1<- n_hf_hs_total_rand
i2<- drop_na(i1)
i2$DTGE<- as.character(i2$DTGE)

i3<- setDT(i2)[DTGE %chin% w2$DTGE]

i4<-setDT( i3 )[ , counter := seq_len( .N ), by = DTGE ]
i4$newcolumn<- paste(i4$DTGE, i4$"counter")
colnames(i4)[colnames(i4)=="newcolumn"] <- "DTGEC"

r6<-merge(r5, i4[, c("DTGEC", "total")], by="DTGEC", all.x = TRUE)

colnames(r6)[colnames(r6)=="Col1"] <- "Label"

write.csv(r6, "HF_HS_samples_key_w_phen_rand.csv")

# Updating abundance tables
# phylum
t1<-hfhs_trans_zotu_normalized_sum_phylum

y1<- merge(t1, r6[, c("Label", "trig", "gluc","prot","weight","dev","total")], by="Label", all.x = TRUE)

write.csv(y1, "z_n_phen_rand_p.csv")

# class
t1<-hfhs_trans_zotu_normalized_sum_class

y1<- merge(t1, r6[, c("Label", "trig", "gluc","prot","weight","dev","total")], by="Label", all.x = TRUE)

write.csv(y1, "z_n_phen_rand_c.csv")
# order
t1<-hfhs_trans_zotu_normalized_sum_order

y1<- merge(t1, r6[, c("Label", "trig", "gluc","prot","weight","dev","total")], by="Label", all.x = TRUE)

write.csv(y1, "z_n_phen_rand_o.csv")
# family
t1<-hfhs_trans_zotu_normalized_sum_family

y1<- merge(t1, r6[, c("Label", "trig", "gluc","prot","weight","dev","total")], by="Label", all.x = TRUE)

write.csv(y1, "z_n_phen_rand_f.csv")
# genus
t1<-hfhs_trans_zotu_normalized_sum_genera

y1<- merge(t1, r6[, c("Label", "trig", "gluc","prot","weight","dev","total")], by="Label", all.x = TRUE)

write.csv(y1, "z_n_phen_rand_g.csv")
# species 
t1<-hfhs_trans_zotu_normalized_sum_species

y1<- merge(t1, r6[, c("Label", "trig", "gluc","prot","weight","dev","total")], by="Label", all.x = TRUE)

write.csv(y1, "z_n_phen_rand_s.csv")

# zotu
t1<-transpose_all_zotu_norm

y1<- merge(t1, r6[, c("Label", "trig", "gluc","prot","weight","dev","total")], by="Label", all.x = TRUE)

write.csv(y1, "z_n_phen_rand_z.csv")




