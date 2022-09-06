library(Hmisc)
library(rstatix)
library(dplyr)
library(gtools)
library(data.table)
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
} 

q<- transpose_all_zotu_norm
q1<-q
# HF NS 
w<- q1[(q1$DT=="RHFNS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a3<- drop_na(a2)
a4<- adjust_pvalue(a3, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
a5$newcolumn<- unique(w$Diet)
colnames(a5)[colnames(a5)=="newcolumn"] <- "Diet"
a5$newcolumn<- unique(w$"Treatment")
colnames(a5)[colnames(a5)=="newcolumn"] <- "Treatment"
e1<-a5

x1.01<- a5[a5$p < 0.05, ]
x1.02<- a5[a5$"fdr p" < 0.1, ]
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$newcolumn<-paste(unique(w$DT))
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o1<-i1

w<- q1[(q1$DT=="PHFNS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a3<- drop_na(a2)
a4<- adjust_pvalue(a3, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
a5$newcolumn<- unique(w$Diet)
colnames(a5)[colnames(a5)=="newcolumn"] <- "Diet"
a5$newcolumn<- unique(w$"Treatment")
colnames(a5)[colnames(a5)=="newcolumn"] <- "Treatment"
e2<-a5

x1.01<- a5[a5$p < 0.05, ]
x1.02<- a5[a5$"fdr p" < 0.1, ]
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$newcolumn<-paste(unique(w$DT))
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o2<-i1

w<- q1[(q1$DT=="PHFANS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a3<- drop_na(a2)
a4<- adjust_pvalue(a3, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
a5$newcolumn<- unique(w$Diet)
colnames(a5)[colnames(a5)=="newcolumn"] <- "Diet"
a5$newcolumn<- unique(w$"Treatment")
colnames(a5)[colnames(a5)=="newcolumn"] <- "Treatment"
e3<-a5

x1.01<- a5[a5$p < 0.05, ]
x1.02<- a5[a5$"fdr p" < 0.1, ]
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$newcolumn<-paste(unique(w$DT))
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o3<-i1
# HF S

w<- q1[(q1$DT=="RHFS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a3<- drop_na(a2)
a4<- adjust_pvalue(a3, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
a5$newcolumn<- unique(w$Diet)
colnames(a5)[colnames(a5)=="newcolumn"] <- "Diet"
a5$newcolumn<- unique(w$"Treatment")
colnames(a5)[colnames(a5)=="newcolumn"] <- "Treatment"
e4<-a5

x1.01<- a5[a5$p < 0.05, ]
x1.02<- a5[a5$"fdr p" < 0.1, ]
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$newcolumn<-paste(unique(w$DT))
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o4<-i1

w<- q1[(q1$DT=="PHFS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a3<- drop_na(a2)
a4<- adjust_pvalue(a3, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
a5$newcolumn<- unique(w$Diet)
colnames(a5)[colnames(a5)=="newcolumn"] <- "Diet"
a5$newcolumn<- unique(w$"Treatment")
colnames(a5)[colnames(a5)=="newcolumn"] <- "Treatment"
e5<-a5

x1.01<- a5[a5$p < 0.05, ]
x1.02<- a5[a5$"fdr p" < 0.1, ]
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$newcolumn<-paste(unique(w$DT))
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o5<-i1

w<- q1[(q1$DT=="PHFAS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a3<- drop_na(a2)
a4<- adjust_pvalue(a3, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
a5$newcolumn<- unique(w$Diet)
colnames(a5)[colnames(a5)=="newcolumn"] <- "Diet"
a5$newcolumn<- unique(w$"Treatment")
colnames(a5)[colnames(a5)=="newcolumn"] <- "Treatment"
e6<-a5

x1.01<- a5[a5$p < 0.05, ]
x1.02<- a5[a5$"fdr p" < 0.1, ]
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$newcolumn<-paste(unique(w$DT))
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o6<-i1

r1<- rbind(e1,e2,e3,e4,e5,e6)



r1$newcolumn<- "HF"
colnames(r1)[colnames(r1)=="newcolumn"] <- "Type"

t1<- rbind(o1,o2,o3,o4,o5,o6)
t1$newcolumn<- "HF"
colnames(t1)[colnames(t1)=="newcolumn"] <- "Type"

# HS NS 

w<- q1[(q1$DT=="RHSNS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a3<- drop_na(a2)
a4<- adjust_pvalue(a3, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
a5$newcolumn<- unique(w$Diet)
colnames(a5)[colnames(a5)=="newcolumn"] <- "Diet"
a5$newcolumn<- unique(w$"Treatment")
colnames(a5)[colnames(a5)=="newcolumn"] <- "Treatment"
e1<-a5

x1.01<- a5[a5$p < 0.05, ]
x1.02<- a5[a5$"fdr p" < 0.1, ]
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$newcolumn<-paste(unique(w$DT))
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o1<-i1

w<- q1[(q1$DT=="PHS13NS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a3<- drop_na(a2)
a4<- adjust_pvalue(a3, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
a5$newcolumn<- unique(w$Diet)
colnames(a5)[colnames(a5)=="newcolumn"] <- "Diet"
a5$newcolumn<- unique(w$"Treatment")
colnames(a5)[colnames(a5)=="newcolumn"] <- "Treatment"
e2<-a5

x1.01<- a5[a5$p < 0.05, ]
x1.02<- a5[a5$"fdr p" < 0.1, ]
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$newcolumn<-paste(unique(w$DT))
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o2<-i1

w<- q1[(q1$DT=="PHSA13NS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a3<- drop_na(a2)
a4<- adjust_pvalue(a3, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
a5$newcolumn<- unique(w$Diet)
colnames(a5)[colnames(a5)=="newcolumn"] <- "Diet"
a5$newcolumn<- unique(w$"Treatment")
colnames(a5)[colnames(a5)=="newcolumn"] <- "Treatment"
e3<-a5

x1.01<- a5[a5$p < 0.05, ]
x1.02<- a5[a5$"fdr p" < 0.1, ]
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$newcolumn<-paste(unique(w$DT))
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o3<-i1

w<- q1[(q1$DT=="PHSA6NS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a3<- drop_na(a2)
a4<- adjust_pvalue(a3, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
a5$newcolumn<- unique(w$Diet)
colnames(a5)[colnames(a5)=="newcolumn"] <- "Diet"
a5$newcolumn<- unique(w$"Treatment")
colnames(a5)[colnames(a5)=="newcolumn"] <- "Treatment"
e4<-a5

x1.01<- a5[a5$p < 0.05, ]
x1.02<- a5[a5$"fdr p" < 0.1, ]
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$newcolumn<-paste(unique(w$DT))
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o4<-i1

#HS S

w<- q1[(q1$DT=="RHSS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a3<- drop_na(a2)
a4<- adjust_pvalue(a3, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
a5$newcolumn<- unique(w$Diet)
colnames(a5)[colnames(a5)=="newcolumn"] <- "Diet"
a5$newcolumn<- unique(w$"Treatment")
colnames(a5)[colnames(a5)=="newcolumn"] <- "Treatment"
e5<-a5

x1.01<- a5[a5$p < 0.05, ]
x1.02<- a5[a5$"fdr p" < 0.1, ]
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$newcolumn<-paste(unique(w$DT))
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o5<-i1

w<- q1[(q1$DT=="PHS13S"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a3<- drop_na(a2)
a4<- adjust_pvalue(a3, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
a5$newcolumn<- unique(w$Diet)
colnames(a5)[colnames(a5)=="newcolumn"] <- "Diet"
a5$newcolumn<- unique(w$"Treatment")
colnames(a5)[colnames(a5)=="newcolumn"] <- "Treatment"
e6<-a5

x1.01<- a5[a5$p < 0.05, ]
x1.02<- a5[a5$"fdr p" < 0.1, ]
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$newcolumn<-paste(unique(w$DT))
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o6<-i1

r2<- rbind(e1,e2,e3,e4,e5,e6)
r2$newcolumn<- "HS"
colnames(r2)[colnames(r2)=="newcolumn"] <- "Type"

t2<- rbind(o1,o2,o3,o4,o5,o6)
t2$newcolumn<- "HS"
colnames(t2)[colnames(t2)=="newcolumn"] <- "Type"

# Controls
w<- q1[(q1$DT=="RNS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a3<- drop_na(a2)
a4<- adjust_pvalue(a3, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
a5$newcolumn<- unique(w$Diet)
colnames(a5)[colnames(a5)=="newcolumn"] <- "Diet"
a5$newcolumn<- unique(w$"Treatment")
colnames(a5)[colnames(a5)=="newcolumn"] <- "Treatment"
e1<-a5

x1.01<- a5[a5$p < 0.05, ]
x1.02<- a5[a5$"fdr p" < 0.1, ]
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$newcolumn<-paste(unique(w$DT))
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o1<-i1

w<- q1[(q1$DT=="PRNS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a3<- drop_na(a2)
a4<- adjust_pvalue(a3, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
a5$newcolumn<- unique(w$Diet)
colnames(a5)[colnames(a5)=="newcolumn"] <- "Diet"
a5$newcolumn<- unique(w$"Treatment")
colnames(a5)[colnames(a5)=="newcolumn"] <- "Treatment"
e2<-a5

x1.01<- a5[a5$p < 0.05, ]
x1.02<- a5[a5$"fdr p" < 0.1, ]
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
i1$newcolumn<-paste(unique(w$DT))
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o2<-i1

r3<- rbind(e1,e2)
r3$newcolumn<- "N"
colnames(r3)[colnames(r3)=="newcolumn"] <- "Type"

t3<- rbind(o1,o2)
t3$newcolumn<- "N"
colnames(t3)[colnames(t3)=="newcolumn"] <- "Type"

y1<- rbind(r1,r2,r3)
colnames(y1)[colnames(y1)=="row"] <- "Taxa"
colnames(y1)[colnames(y1)=="column"] <- "Taxa"
colnames(y1)[colnames(y1)=="cor"] <- "Correlation"
y1$Diet <- gsub("13", "11", y1$Diet)
write.csv(y1, "spear_microb_hfhs_z_norm_s.csv")

y2<- rbind(t1,t2,t3)
y2$comparison <- gsub("13", "11", y2$comparison)
write.csv(y2, "signp_spear_microb_hfhs_z_norm_z.csv")


