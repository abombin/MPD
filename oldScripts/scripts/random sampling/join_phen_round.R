w1<- HF_HS_samples_key_updated_for_sequences_results
e1<- w1[(w1$"Type"=="HF"),]
e1$newcolumn<-"e1"
colnames(e1)[colnames(e1)=="newcolumn"] <- "Experiment"


e2<- w1[(w1$"Type"=="HS"),]
e2$newcolumn<-"e2"
colnames(e2)[colnames(e2)=="newcolumn"] <- "Experiment"

e3<- w1[(w1$"Type"=="N"),]
e3$newcolumn<-"e2"
colnames(e3)[colnames(e3)=="newcolumn"] <- "Experiment"

w2<- rbind(e1,e2,e3)

w2$newcolumn<- paste(w2$Diet,w2$Treatment, w2$"Genetic line", w2$Round, w2$Experiment)
colnames(w2)[colnames(w2)=="newcolumn"] <- "dtgre"

#total
i2<- t1
r1<-merge(w2, i2[, c("dtgre", "total")], by="dtgre", all.x = TRUE)

# dev
i2<- t2
r2<-merge(r1, i2[, c("dtgre", "dev")], by="dtgre", all.x = TRUE)

# weight
i2<- t3
r3<-merge(r2, i2[, c("dtgre", "weight")], by="dtgre", all.x = TRUE)

# trig
i2<- t4
r4<-merge(r3, i2[, c("dtgre", "trig")], by="dtgre", all.x = TRUE)

#gluc
i2<- t5
r5<-merge(r4, i2[, c("dtgre", "gluc")], by="dtgre", all.x = TRUE)

#prot
i2<- t6
r6<-merge(r5, i2[, c("dtgre", "prot")], by="dtgre", all.x = TRUE)

write.csv(r6, "HF_HS_samples_key_w_phen_round.csv")

# updating with taxa
t1<-hfhs_trans_zotu_normalized_sum_phylum

y1<- merge(t1, r6[, c("Label", "trig", "gluc","prot","weight","dev","total")], by="Label", all.x = TRUE)

write.csv(y1, "z_n_phen_round_p.csv")

# class
t1<-hfhs_trans_zotu_normalized_sum_class

y1<- merge(t1, r6[, c("Label", "trig", "gluc","prot","weight","dev","total")], by="Label", all.x = TRUE)

write.csv(y1, "z_n_phen_round_c.csv")
# order
t1<-hfhs_trans_zotu_normalized_sum_order

y1<- merge(t1, r6[, c("Label", "trig", "gluc","prot","weight","dev","total")], by="Label", all.x = TRUE)

write.csv(y1, "z_n_phen_round_o.csv")
# family
t1<-hfhs_trans_zotu_normalized_sum_family

y1<- merge(t1, r6[, c("Label", "trig", "gluc","prot","weight","dev","total")], by="Label", all.x = TRUE)

write.csv(y1, "z_n_phen_round_f.csv")
# genus
t1<-hfhs_trans_zotu_normalized_sum_genera

y1<- merge(t1, r6[, c("Label", "trig", "gluc","prot","weight","dev","total")], by="Label", all.x = TRUE)

write.csv(y1, "z_n_phen_round_g.csv")
# species 
t1<-hfhs_trans_zotu_normalized_sum_species

y1<- merge(t1, r6[, c("Label", "trig", "gluc","prot","weight","dev","total")], by="Label", all.x = TRUE)

write.csv(y1, "z_n_phen_round_s.csv")

# zotu
t1<-transpose_all_zotu_norm

y1<- merge(t1, r6[, c("Label", "trig", "gluc","prot","weight","dev","total")], by="Label", all.x = TRUE)

write.csv(y1, "z_n_phen_round_z.csv")
