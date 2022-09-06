library(dplyr)
q1<-tr_z_n_phen_rand_g
q2<-q1[(q1$"taxa"=="Acetobacter"| q1$"taxa"=="Lactobacillus"),]
colnames(q2)[3] <- "Genotype"
w1<-cor.test(q2$abundance, q2$trig, method= "spearman")

e2 <- q2 %>% group_by(taxa, Diet, Genotype) %>% do(model = cor.test(q2$abundance, q2$trig, method= "pearson"))
e3<-as.data.frame(sapply(e2$model, function(x) x$"p.value"))
e4<-as.data.frame(t(e3))
w1<-as.data.frame(e2$taxa)

e2 <- q2 %>% group_by(taxa, Diet, Genotype) %>% do(model = cor.test(q2$abundance, q2$trig, method= "pearson"))
e3<-as.data.frame(sapply(e2$model, function(x) x$"p.value"))

q3<-q2[!(q2$"Diet"=="PHSA13"),]
m1<-data.frame(table(q2$Diet, q2$Genotype))

a3<-q2%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, total, method="pearson")$p.val)

### clean HF
q1<-tr_z_n_phen_rand_g
q2<-q1[(q1$"taxa"=="Acetobacter"| q1$"taxa"=="Lactobacillus"),]
colnames(q2)[3] <- "Genotype"
q3<-q2[(q2$"Type"=="HF"),]
a3<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, total, method="pearson")$p.val)

### HS
q1<-tr_z_n_phen_rand_g
q2<-q1[(q1$"taxa"=="Acetobacter"| q1$"taxa"=="Lactobacillus"),]
colnames(q2)[3] <- "Genotype"
q3<-q2[(q2$"Type"=="HS"),]
q4<-q3[!(q3$Diet=="PHSA13"|q3$Diet=="PHSA6"),]
a3<-q4%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, total, method="pearson")$p.val)

####
q3<-q2 %>% group_by(taxa, Diet, Genotype) %>% filter(n() >= 4)
a3<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, gluc, method="spearman")$p.val)
a4<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, gluc, method="spearman")$estimate)
colnames(a4)[4]<-"cor"
colnames(a3)[4]<-"p"
a3$cor<-a4$cor
