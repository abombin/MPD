library(dplyr)
library(rstatix)
library(utils)
library(plyr)

q<-tr_z_n_phen_rand_z
q1<- q
colnames(q1)[colnames(q1)=="Treatment"] <- "t"
colnames(q1)[colnames(q1)=="Genetic line"] <- "g"
colnames(q1)[colnames(q1)=="Diet"] <- "d"
colnames(q1)[colnames(q1)=="Round"] <- "r"

### Abundance*Diet

### trig
e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(trig~abundance*d,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(trig~abundance*d,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Triglyceride"
x3$phenotype<-"Triglyceride"
t1<-w5
r1<-x3


### gluc

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(gluc~abundance*d,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(gluc~abundance*d,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Glucose"
x3$phenotype<-"Glucose"

t2<-w5
r2<-x3

### prot

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(prot~abundance*d,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(prot~abundance*d,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Protein"
x3$phenotype<-"Protein"
t3<-w5
r3<-x3

### weight

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(weight~abundance*d,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(weight~abundance*d,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Weight"
x3$phenotype<-"Weight"
t4<-w5
r4<-x3

### dev


e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(dev~abundance*d,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(dev~abundance*d,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Development"
x3$phenotype<-"Development"
t5<-w5
r5<-x3

### total

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(total~abundance*d,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(total~abundance*d,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Total"
x3$phenotype<-"Total"
t6<-w5
r6<-x3

y1<-rbind.fill(t1,t2,t3,t4,t5,t6)
u1<-rbind.fill(r1,r2,r3,r4,r5,r6)




colnames(y1)[1] <- "Taxa"
colnames(y1)[3]<- "Abundance p"
colnames(y1)[4]<- "Diet p"
colnames(y1)[5]<- "A*D p"
colnames(y1)[6]<- "Abundance fdrp"
colnames(y1)[7]<- "Diet fdrp"
colnames(y1)[8]<- "A*D fdrp"
colnames(y1)[9]<- "Phenotype"


x1.01<- y1[y1[,3] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,3]), ]
x1.02<- x1.01a[x1.01a[,6] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Phenotype<-"All"
i1$comparison<-colnames(y1)[3]
o1<-i1

x1.01<- y1[y1[,4] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,4]), ]
x1.02<- x1.01a[x1.01a[,7] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Phenotype<-"All"
i1$comparison<-colnames(y1)[4]
o2<-i1

x1.01<- y1[y1[,5] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,5]), ]
x1.02<- x1.01a[x1.01a[,8] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Phenotype<-"All"
i1$comparison<-colnames(y1)[5]
o3<-i1

o4<-rbind(o1,o2,o3)
o4$comparison<-gsub("\\ p", "", o4$comparison)

y1a<-select(y1, -c(V1))

write.csv(y1a, "qp_z_n_ad_z.csv")
write.csv(o4, "qp_#sign_z_n_ad_z.csv")
write.csv(u1, "qp_model_z_n_ad_z.csv")


### Abundance*Genotype

### trig

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(trig~abundance*g,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(trig~abundance*g,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Triglyceride"
x3$phenotype<-"Triglyceride"
t1<-w5
r1<-x3

### gluc

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(gluc~abundance*g,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(gluc~abundance*g,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Glucose"
x3$phenotype<-"Glucose"
t2<-w5
r2<-x3

### prot

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(prot~abundance*g,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(prot~abundance*g,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Protein"
x3$phenotype<-"Protein"
t3<-w5
r3<-x3

### weight

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(weight~abundance*g,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(weight~abundance*g,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Weight"
x3$phenotype<-"Weight"
t4<-w5
r4<-x3

### dev


e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(dev~abundance*g,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(dev~abundance*g,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Development"
x3$phenotype<-"Development"
t5<-w5
r5<-x3

### total

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(total~abundance*g,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(total~abundance*g,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Total"
x3$phenotype<-"Total"
t6<-w5
r6<-x3

y1<-rbind.fill(t1,t2,t3,t4,t5,t6)
u1<-rbind.fill(r1,r2,r3,r4,r5,r6)




colnames(y1)[1] <- "Taxa"
colnames(y1)[3]<- "Abundance p"
colnames(y1)[4]<- "Genotype p"
colnames(y1)[5]<- "A*G p"
colnames(y1)[6]<- "Abundance fdrp"
colnames(y1)[7]<- "Genotype fdrp"
colnames(y1)[8]<- "A*G fdrp"
colnames(y1)[9]<- "Phenotype"


x1.01<- y1[y1[,3] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,3]), ]
x1.02<- x1.01a[x1.01a[,6] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Phenotype<-"All"
i1$comparison<-colnames(y1)[3]
o1<-i1

x1.01<- y1[y1[,4] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,4]), ]
x1.02<- x1.01a[x1.01a[,7] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Phenotype<-"All"
i1$comparison<-colnames(y1)[4]
o2<-i1

x1.01<- y1[y1[,5] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,5]), ]
x1.02<- x1.01a[x1.01a[,8] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Phenotype<-"All"
i1$comparison<-colnames(y1)[5]
o3<-i1

o4<-rbind(o1,o2,o3)
o4$comparison<-gsub("\\ p", "", o4$comparison)

y1a<-select(y1, -c(V1))

write.csv(y1a, "qp_z_n_ag_z.csv")
write.csv(o4, "qp_#sign_z_n_ag_z.csv")
write.csv(u1, "qp_model_z_n_ag_z.csv")


### Abundance*Treatment

### trig

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(trig~abundance*t,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(trig~abundance*t,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Triglyceride"
x3$phenotype<-"Triglyceride"
t1<-w5
r1<-x3

### gluc

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(gluc~abundance*t,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(gluc~abundance*t,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Glucose"
x3$phenotype<-"Glucose"
t2<-w5
r2<-x3

### prot

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(prot~abundance*t,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(prot~abundance*t,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Protein"
x3$phenotype<-"Protein"
t3<-w5
r3<-x3

### weight

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(weight~abundance*t,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(weight~abundance*t,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Weight"
x3$phenotype<-"Weight"
t4<-w5
r4<-x3

### dev


e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(dev~abundance*t,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(dev~abundance*t,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Development"
x3$phenotype<-"Development"
t5<-w5
r5<-x3

### total

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(total~abundance*t,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(total~abundance*t,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w5$phenotype<-"Total"
x3$phenotype<-"Total"
t6<-w5
r6<-x3

y1<-rbind.fill(t1,t2,t3,t4,t5,t6)
u1<-rbind.fill(r1,r2,r3,r4,r5,r6)




colnames(y1)[1] <- "Taxa"
colnames(y1)[3]<- "Abundance p"
colnames(y1)[4]<- "Treatment p"
colnames(y1)[5]<- "A*T p"
colnames(y1)[6]<- "Abundance fdrp"
colnames(y1)[7]<- "Treatment fdrp"
colnames(y1)[8]<- "A*T fdrp"
colnames(y1)[9]<- "Phenotype"


x1.01<- y1[y1[,3] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,3]), ]
x1.02<- x1.01a[x1.01a[,6] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Phenotype<-"All"
i1$comparison<-colnames(y1)[3]
o1<-i1

x1.01<- y1[y1[,4] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,4]), ]
x1.02<- x1.01a[x1.01a[,7] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Phenotype<-"All"
i1$comparison<-colnames(y1)[4]
o2<-i1

x1.01<- y1[y1[,5] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,5]), ]
x1.02<- x1.01a[x1.01a[,8] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Phenotype<-"All"
i1$comparison<-colnames(y1)[5]
o3<-i1

o4<-rbind(o1,o2,o3)
o4$comparison<-gsub("\\ p", "", o4$comparison)

y1a<-select(y1, -c(V1))

write.csv(y1a, "qp_z_n_at_z.csv")
write.csv(o4, "qp_#sign_z_n_at_z.csv")
write.csv(u1, "qp_model_z_n_at_z.csv")
