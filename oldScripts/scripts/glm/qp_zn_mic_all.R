library(dplyr)
library(rstatix)
library(utils)
library(plyr)

q<-tr_z_n_phen_rand_p
q1<- q
colnames(q1)[colnames(q1)=="Treatment"] <- "t"
colnames(q1)[colnames(q1)=="Genetic line"] <- "g"
colnames(q1)[colnames(q1)=="Diet"] <- "d"
colnames(q1)[colnames(q1)=="Round"] <- "r"

w5 <- q1 %>% group_by(taxa) %>% do(model = aov(abundance~d*g*t, data = .))

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(abundance~d*g*t,family=quasipoisson, data = .,),test="F"))  #does not work since glm models can have all categorical variables and independnat variables
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)
w2<-cbind(w1,e4)
w3<- adjust_pvalue(w2, p.col= "V2", output.col= "V2_fdrp", method= "fdr")
w4<- adjust_pvalue(w3, p.col= "V3", output.col= "V3_fdrp", method= "fdr")
w5<- adjust_pvalue(w4, p.col= "V4", output.col= "V4_fdrp", method= "fdr")
w6<- adjust_pvalue(w5, p.col= "V5", output.col= "V5_fdrp", method= "fdr")
w7<- adjust_pvalue(w6, p.col= "V5", output.col= "V5_fdrp", method= "fdr")

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(abundance~d*g*t,family=quasipoisson, data = .,))
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