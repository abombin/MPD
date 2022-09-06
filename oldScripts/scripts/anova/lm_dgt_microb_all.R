library(dplyr)
library(rstatix)
library(utils)
library(plyr)

q<-tr_z_n_phen_rand_z
q1a<- q
colnames(q1a)[colnames(q1a)=="Genetic.line"]<- "Genetic line"
### HF
q1<-q1a[(q1a$Type=="HF"),]
colnames(q1)[colnames(q1)=="Treatment"] <- "t"
colnames(q1)[colnames(q1)=="Genetic line"] <- "g"
colnames(q1)[colnames(q1)=="Diet"] <- "d"
colnames(q1)[colnames(q1)=="Round"] <- "r"
q1$sra <-sqrt(q1$abundance)

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(lm(sra~d*g*t, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)

w2a<- adjust_pvalue(w2, p.col= "V1", output.col= "V1_fdrp", method= "fdr")
w3<- adjust_pvalue(w2a, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")
w6<- adjust_pvalue(w5, p.col= "V5", output.col= "V5_fdrp", method= "fdr")
w7<- adjust_pvalue(w6, p.col= "V6", output.col= "V6_fdrp", method= "fdr")
w8<- adjust_pvalue(w7, p.col= "V7", output.col= "V7_fdrp", method= "fdr")

x6<-as.data.frame(capture.output(print(e2$model)))
colnames(x6)[1] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w9<-w1
x3<-join(x1, w9, by = "id", type = "left", match = "first")

t1<-w8
r1<-x3

y1 <- t1[ -c(9) ]

colnames(y1)[1] <- "Taxa"
colnames(y1)[2]<- "Diet p"
colnames(y1)[3]<- "Genotype p"
colnames(y1)[4]<- "Treatment p"
colnames(y1)[5]<- "D*G p"
colnames(y1)[6]<- "D*T p"
colnames(y1)[7]<- "G*T p"
colnames(y1)[8]<- "D*G*T p"
colnames(y1)[9]<- "Diet fdrp"
colnames(y1)[10]<- "Genotype fdrp"
colnames(y1)[11]<- "Treatment fdrp"
colnames(y1)[12]<- "D*G fdrp"
colnames(y1)[13]<- "D*T fdrp"
colnames(y1)[14]<- "G*T fdrp"
colnames(y1)[15]<- "D*G*T fdrp"

x1.01<- y1[y1[,2] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,2]), ]
x1.02<- x1.01a[x1.01a[,9] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Variable<-colnames(y1)[2]
o1<-i1

x1.01<- y1[y1[,3] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,3]), ]
x1.02<- x1.01a[x1.01a[,10] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Variable<-colnames(y1)[3]
o2<-i1

x1.01<- y1[y1[,4] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,4]), ]
x1.02<- x1.01a[x1.01a[,11] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Variable<-colnames(y1)[4]
o3<-i1

x1.01<- y1[y1[,5] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,5]), ]
x1.02<- x1.01a[x1.01a[,12] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Variable<-colnames(y1)[5]
o4<-i1

x1.01<- y1[y1[,6] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,6]), ]
x1.02<- x1.01a[x1.01a[,13] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Variable<-colnames(y1)[6]
o5<-i1

x1.01<- y1[y1[,7] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,7]), ]
x1.02<- x1.01a[x1.01a[,14] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Variable<-colnames(y1)[7]
o6<-i1

x1.01<- y1[y1[,8] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,8]), ]
x1.02<- x1.01a[x1.01a[,15] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Variable<-colnames(y1)[8]
o7<-i1

o8<-rbind(o1,o2,o3,o4,o5,o6,o7)
o8$Variable<-gsub("\\ p", "", o8$Variable)

u1<-y1
u1$Type<-"HF"
a1<-r1
a1$Type<-"HF"
p1<-o8
p1$Type<-"HF"


### HS
q1<-q1a[(q1a$Type=="HS"),]
colnames(q1)[colnames(q1)=="Treatment"] <- "t"
colnames(q1)[colnames(q1)=="Genetic line"] <- "g"
colnames(q1)[colnames(q1)=="Diet"] <- "d"
colnames(q1)[colnames(q1)=="Round"] <- "r"
q1$sra <-sqrt(q1$abundance)

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(lm(sra~d*g*t, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)

w2a<- adjust_pvalue(w2, p.col= "V1", output.col= "V1_fdrp", method= "fdr")
w3<- adjust_pvalue(w2a, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")
w6<- adjust_pvalue(w5, p.col= "V5", output.col= "V5_fdrp", method= "fdr")
w7<- adjust_pvalue(w6, p.col= "V6", output.col= "V6_fdrp", method= "fdr")
w8<- adjust_pvalue(w7, p.col= "V7", output.col= "V7_fdrp", method= "fdr")

x6<-as.data.frame(capture.output(print(e2$model)))
colnames(x6)[1] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w9<-w1
x3<-join(x1, w9, by = "id", type = "left", match = "first")

t1<-w8
r1<-x3

y1 <- t1[ -c(9) ]

colnames(y1)[1] <- "Taxa"
colnames(y1)[2]<- "Diet p"
colnames(y1)[3]<- "Genotype p"
colnames(y1)[4]<- "Treatment p"
colnames(y1)[5]<- "D*G p"
colnames(y1)[6]<- "D*T p"
colnames(y1)[7]<- "G*T p"
colnames(y1)[8]<- "D*G*T p"
colnames(y1)[9]<- "Diet fdrp"
colnames(y1)[10]<- "Genotype fdrp"
colnames(y1)[11]<- "Treatment fdrp"
colnames(y1)[12]<- "D*G fdrp"
colnames(y1)[13]<- "D*T fdrp"
colnames(y1)[14]<- "G*T fdrp"
colnames(y1)[15]<- "D*G*T fdrp"

x1.01<- y1[y1[,2] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,2]), ]
x1.02<- x1.01a[x1.01a[,9] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Variable<-colnames(y1)[2]
o1<-i1

x1.01<- y1[y1[,3] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,3]), ]
x1.02<- x1.01a[x1.01a[,10] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Variable<-colnames(y1)[3]
o2<-i1

x1.01<- y1[y1[,4] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,4]), ]
x1.02<- x1.01a[x1.01a[,11] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Variable<-colnames(y1)[4]
o3<-i1

x1.01<- y1[y1[,5] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,5]), ]
x1.02<- x1.01a[x1.01a[,12] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Variable<-colnames(y1)[5]
o4<-i1

x1.01<- y1[y1[,6] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,6]), ]
x1.02<- x1.01a[x1.01a[,13] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Variable<-colnames(y1)[6]
o5<-i1

x1.01<- y1[y1[,7] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,7]), ]
x1.02<- x1.01a[x1.01a[,14] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Variable<-colnames(y1)[7]
o6<-i1

x1.01<- y1[y1[,8] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,8]), ]
x1.02<- x1.01a[x1.01a[,15] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Variable<-colnames(y1)[8]
o7<-i1

o8<-rbind(o1,o2,o3,o4,o5,o6,o7)
o8$Variable<-gsub("\\ p", "", o8$Variable)

u2<-y1
u2$Type<-"HS"
a2<-r1
a2$Type<-"HS"
p2<-o8
p2$Type<-"HS"

u3<-rbind(u1,u2)
a3<-rbind(a1,a2)
p3<-rbind(p1,p2)


write.csv(u3, "zn_lm_abund_dgt_inter_hfhs_z.csv")

write.csv(a3, "zn_model_lm_abund_dgt_inter_hfhs_z.csv")

write.csv(p3, "zn_#sign_lm_abund_dgt_inter_hfhs_z.csv")

