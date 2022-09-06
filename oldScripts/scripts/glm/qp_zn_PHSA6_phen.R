library(dplyr)
library(rstatix)
library(utils)
library(plyr)

q<-tr_z_n_phen_rand_g
q1<-q[(q$DT=="PHSA6S"),]
colnames(q1)[colnames(q1)=="Treatment"] <- "t"
colnames(q1)[colnames(q1)=="Genetic line"] <- "g"
colnames(q1)[colnames(q1)=="Diet"] <- "d"
colnames(q1)[colnames(q1)=="Round"] <- "r"

### trig
e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(trig~abundance,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(trig~abundance,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w3$phenotype<-"Triglyceride"
x3$phenotype<-"Triglyceride"
t1<-w3
r1<-x3
### gluc
e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(gluc~abundance,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(gluc~abundance,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w3$phenotype<-"Glucose"
x3$phenotype<-"Glucose"
t2<-w3
r2<-x3
### prot
e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(prot~abundance,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(prot~abundance,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w3$phenotype<-"Protein"
x3$phenotype<-"Protein"
t3<-w3
r3<-x3
### dev
e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(dev~abundance,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(dev~abundance,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w3$phenotype<-"Development"
x3$phenotype<-"Development"
t4<-w3
r4<-x3
### total
e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(total~abundance,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(total~abundance,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w3$phenotype<-"Total"
x3$phenotype<-"Total"
t5<-w3
r5<-x3
### weight

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(weight~abundance,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(weight~abundance,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
x6<-as.data.frame(capture.output(print(e2v1)))
colnames(x6)[colnames(x6)=="capture.output(print(e2v1))"] <- "x"
x1<-as.data.frame(gsub("\\[|\\]", "", x6$x))
colnames(x1)[1] <- "id"
w1$id <- seq.int(nrow(w1))
colnames(w1)[1] <- "taxa"
w8<-w1
x3<-join(x1, w8, by = "id", type = "left", match = "first")
w3$phenotype<-"Weight"
x3$phenotype<-"Weight"
t6<-w3
r6<-x3

y1<-rbind.fill(t1,t2,t3,t4,t5,t6)
u1<-rbind.fill(r1,r2,r3,r4,r5,r6)

colnames(y1)[1] <- "Taxa"
colnames(y1)[3]<- "p.value"
colnames(y1)[4]<- "fdr.p"

x1.01<- y1[y1[,3] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,3]), ]
x1.02<- x1.01a[x1.01a[,4] < 0.1, ]
i<-nrow(x1.01a)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$Phenotype<-"All"
o1<-i1

y1a<-select(y1, -c(V1))
y2<-y1a[!is.na(y1a[,2]), ]

write.csv(y2, "qp_zn_a_PHSA6S_g.csv")
write.csv(o1, "qp_#sign_zn_a_PHSA6S_g.csv")
write.csv(u1, "qp_model_zn_a_PHSA6S_g.csv")