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


# data table
q<- z_n_phen_round_z
q1<-q
colnames(q1)[colnames(q1)=="Genetic.line"] <- "Genetic line"
colnames(q1)[colnames(q1)=="Label.2"] <- "Label 2"


# HF NS 
w<- q1[(q1$DT=="RHFNS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample, X))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a2a<-a2[(a2$column=="total" | a2$column=="dev" | a2$column=="weight" | a2$column=="trig" | a2$column=="prot" | a2$column=="gluc"),]
a2<-a2a[!(a2a$row=="total" | a2a$row=="dev" | a2a$row=="weight" | a2a$row=="trig" | a2a$row=="prot" | a2a$row=="gluc"),]
a3<- drop_na(a2)

a3$newcolumn<- unique(w$Diet)
colnames(a3)[colnames(a3)=="newcolumn"] <- "Diet"
a3$newcolumn<- unique(w$"Treatment")
colnames(a3)[colnames(a3)=="newcolumn"] <- "Treatment"

a3a<-filter(a3, column=="trig")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o1<-i1

a3a<-filter(a3, column=="gluc")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o2<-i1

a3a<-filter(a3, column=="prot")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o3<-i1

a3a<-filter(a3, column=="weight")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o4<-i1

a3a<-filter(a3, column=="dev")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o5<-i1

a3a<-filter(a3, column=="total")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o6<-i1

r1<-rbind(e1, e2,e3,e4,e5,e6)
t1<-rbind(o1,o2,o3,o4,o5,o6)

p1<-rbind(o1,o2,o3,o4,o5,o6)
x1.01<-sum(p1$`# sign p`)
x1.02<- sum(p1$`# sign fdrp`)
s1<-as.data.frame(cbind(x1.01,x1.02))
colnames(s1)[colnames(s1)=="x1.01"] <- "# sign p"
colnames(s1)[colnames(s1)=="x1.02"] <- "# sign fdrp"
s1$newcolumn<-paste(unique(w$DT))
colnames(s1)[colnames(s1)=="newcolumn"] <- "comparison"
d1<-s1

###
w<- q1[(q1$DT=="PHFNS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample, X))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a2a<-a2[(a2$column=="total" | a2$column=="dev" | a2$column=="weight" | a2$column=="trig" | a2$column=="prot" | a2$column=="gluc"),]
a2<-a2a[!(a2a$row=="total" | a2a$row=="dev" | a2a$row=="weight" | a2a$row=="trig" | a2a$row=="prot" | a2a$row=="gluc"),]
a3<- drop_na(a2)

a3$newcolumn<- unique(w$Diet)
colnames(a3)[colnames(a3)=="newcolumn"] <- "Diet"
a3$newcolumn<- unique(w$"Treatment")
colnames(a3)[colnames(a3)=="newcolumn"] <- "Treatment"

a3a<-filter(a3, column=="trig")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o1<-i1

a3a<-filter(a3, column=="gluc")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o2<-i1

a3a<-filter(a3, column=="prot")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o3<-i1

a3a<-filter(a3, column=="weight")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o4<-i1

a3a<-filter(a3, column=="dev")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o5<-i1

a3a<-filter(a3, column=="total")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o6<-i1

r2<-rbind(e1, e2,e3,e4,e5,e6)
t2<-rbind(o1,o2,o3,o4,o5,o6)

p1<-rbind(o1,o2,o3,o4,o5,o6)
x1.01<-sum(p1$`# sign p`)
x1.02<- sum(p1$`# sign fdrp`)
s1<-as.data.frame(cbind(x1.01,x1.02))
colnames(s1)[colnames(s1)=="x1.01"] <- "# sign p"
colnames(s1)[colnames(s1)=="x1.02"] <- "# sign fdrp"
s1$newcolumn<-paste(unique(w$DT))
colnames(s1)[colnames(s1)=="newcolumn"] <- "comparison"
d2<-s1

###
w<- q1[(q1$DT=="PHFANS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample, X))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a2a<-a2[(a2$column=="total" | a2$column=="dev" | a2$column=="weight" | a2$column=="trig" | a2$column=="prot" | a2$column=="gluc"),]
a2<-a2a[!(a2a$row=="total" | a2a$row=="dev" | a2a$row=="weight" | a2a$row=="trig" | a2a$row=="prot" | a2a$row=="gluc"),]
a3<- drop_na(a2)

a3$newcolumn<- unique(w$Diet)
colnames(a3)[colnames(a3)=="newcolumn"] <- "Diet"
a3$newcolumn<- unique(w$"Treatment")
colnames(a3)[colnames(a3)=="newcolumn"] <- "Treatment"

a3a<-filter(a3, column=="trig")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o1<-i1

a3a<-filter(a3, column=="gluc")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o2<-i1

a3a<-filter(a3, column=="prot")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o3<-i1

a3a<-filter(a3, column=="weight")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o4<-i1

a3a<-filter(a3, column=="dev")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o5<-i1

a3a<-filter(a3, column=="total")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o6<-i1

r3<-rbind(e1, e2,e3,e4,e5,e6)
t3<-rbind(o1,o2,o3,o4,o5,o6)

p1<-rbind(o1,o2,o3,o4,o5,o6)
x1.01<-sum(p1$`# sign p`)
x1.02<- sum(p1$`# sign fdrp`)
s1<-as.data.frame(cbind(x1.01,x1.02))
colnames(s1)[colnames(s1)=="x1.01"] <- "# sign p"
colnames(s1)[colnames(s1)=="x1.02"] <- "# sign fdrp"
s1$newcolumn<-paste(unique(w$DT))
colnames(s1)[colnames(s1)=="newcolumn"] <- "comparison"
d3<-s1

### HF S
w<- q1[(q1$DT=="RHFS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample, X))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a2a<-a2[(a2$column=="total" | a2$column=="dev" | a2$column=="weight" | a2$column=="trig" | a2$column=="prot" | a2$column=="gluc"),]
a2<-a2a[!(a2a$row=="total" | a2a$row=="dev" | a2a$row=="weight" | a2a$row=="trig" | a2a$row=="prot" | a2a$row=="gluc"),]
a3<- drop_na(a2)

a3$newcolumn<- unique(w$Diet)
colnames(a3)[colnames(a3)=="newcolumn"] <- "Diet"
a3$newcolumn<- unique(w$"Treatment")
colnames(a3)[colnames(a3)=="newcolumn"] <- "Treatment"

a3a<-filter(a3, column=="trig")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o1<-i1

a3a<-filter(a3, column=="gluc")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o2<-i1

a3a<-filter(a3, column=="prot")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o3<-i1

a3a<-filter(a3, column=="weight")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o4<-i1

a3a<-filter(a3, column=="dev")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o5<-i1

a3a<-filter(a3, column=="total")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o6<-i1

r4<-rbind(e1, e2,e3,e4,e5,e6)
t4<-rbind(o1,o2,o3,o4,o5,o6)

p1<-rbind(o1,o2,o3,o4,o5,o6)
x1.01<-sum(p1$`# sign p`)
x1.02<- sum(p1$`# sign fdrp`)
s1<-as.data.frame(cbind(x1.01,x1.02))
colnames(s1)[colnames(s1)=="x1.01"] <- "# sign p"
colnames(s1)[colnames(s1)=="x1.02"] <- "# sign fdrp"
s1$newcolumn<-paste(unique(w$DT))
colnames(s1)[colnames(s1)=="newcolumn"] <- "comparison"
d4<-s1

###
w<- q1[(q1$DT=="PHFS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample, X))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a2a<-a2[(a2$column=="total" | a2$column=="dev" | a2$column=="weight" | a2$column=="trig" | a2$column=="prot" | a2$column=="gluc"),]
a2<-a2a[!(a2a$row=="total" | a2a$row=="dev" | a2a$row=="weight" | a2a$row=="trig" | a2a$row=="prot" | a2a$row=="gluc"),]
a3<- drop_na(a2)

a3$newcolumn<- unique(w$Diet)
colnames(a3)[colnames(a3)=="newcolumn"] <- "Diet"
a3$newcolumn<- unique(w$"Treatment")
colnames(a3)[colnames(a3)=="newcolumn"] <- "Treatment"

a3a<-filter(a3, column=="trig")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o1<-i1

a3a<-filter(a3, column=="gluc")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o2<-i1

a3a<-filter(a3, column=="prot")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o3<-i1

a3a<-filter(a3, column=="weight")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o4<-i1

a3a<-filter(a3, column=="dev")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o5<-i1

a3a<-filter(a3, column=="total")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o6<-i1

r5<-rbind(e1, e2,e3,e4,e5,e6)
t5<-rbind(o1,o2,o3,o4,o5,o6)

p1<-rbind(o1,o2,o3,o4,o5,o6)
x1.01<-sum(p1$`# sign p`)
x1.02<- sum(p1$`# sign fdrp`)
s1<-as.data.frame(cbind(x1.01,x1.02))
colnames(s1)[colnames(s1)=="x1.01"] <- "# sign p"
colnames(s1)[colnames(s1)=="x1.02"] <- "# sign fdrp"
s1$newcolumn<-paste(unique(w$DT))
colnames(s1)[colnames(s1)=="newcolumn"] <- "comparison"
d5<-s1

###
w<- q1[(q1$DT=="PHFAS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample, X))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a2a<-a2[(a2$column=="total" | a2$column=="dev" | a2$column=="weight" | a2$column=="trig" | a2$column=="prot" | a2$column=="gluc"),]
a2<-a2a[!(a2a$row=="total" | a2a$row=="dev" | a2a$row=="weight" | a2a$row=="trig" | a2a$row=="prot" | a2a$row=="gluc"),]
a3<- drop_na(a2)

a3$newcolumn<- unique(w$Diet)
colnames(a3)[colnames(a3)=="newcolumn"] <- "Diet"
a3$newcolumn<- unique(w$"Treatment")
colnames(a3)[colnames(a3)=="newcolumn"] <- "Treatment"

a3a<-filter(a3, column=="trig")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o1<-i1

a3a<-filter(a3, column=="gluc")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o2<-i1

a3a<-filter(a3, column=="prot")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o3<-i1

a3a<-filter(a3, column=="weight")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o4<-i1

a3a<-filter(a3, column=="dev")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o5<-i1

a3a<-filter(a3, column=="total")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o6<-i1

r6<-rbind(e1, e2,e3,e4,e5,e6)
t6<-rbind(o1,o2,o3,o4,o5,o6)

p1<-rbind(o1,o2,o3,o4,o5,o6)
x1.01<-sum(p1$`# sign p`)
x1.02<- sum(p1$`# sign fdrp`)
s1<-as.data.frame(cbind(x1.01,x1.02))
colnames(s1)[colnames(s1)=="x1.01"] <- "# sign p"
colnames(s1)[colnames(s1)=="x1.02"] <- "# sign fdrp"
s1$newcolumn<-paste(unique(w$DT))
colnames(s1)[colnames(s1)=="newcolumn"] <- "comparison"
d6<-s1

u1<- rbind(r1,r2,r3,r4,r5,r6)
u1$newcolumn<- "HF"
colnames(u1)[colnames(u1)=="newcolumn"] <- "Type"

f1<-rbind(t1,t2,t3,t4,t5,t6)
f1$newcolumn<- "HF"
colnames(f1)[colnames(f1)=="newcolumn"] <- "Type"

g1<- rbind(d1,d2,d3,d4,d5,d6)
g1$newcolumn<- "HF"
colnames(g1)[colnames(g1)=="newcolumn"] <- "Type"

### HS NS
w<- q1[(q1$DT=="RHSNS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample, X))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a2a<-a2[(a2$column=="total" | a2$column=="dev" | a2$column=="weight" | a2$column=="trig" | a2$column=="prot" | a2$column=="gluc"),]
a2<-a2a[!(a2a$row=="total" | a2a$row=="dev" | a2a$row=="weight" | a2a$row=="trig" | a2a$row=="prot" | a2a$row=="gluc"),]
a3<- drop_na(a2)

a3$newcolumn<- unique(w$Diet)
colnames(a3)[colnames(a3)=="newcolumn"] <- "Diet"
a3$newcolumn<- unique(w$"Treatment")
colnames(a3)[colnames(a3)=="newcolumn"] <- "Treatment"

a3a<-filter(a3, column=="trig")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o1<-i1

a3a<-filter(a3, column=="gluc")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o2<-i1

a3a<-filter(a3, column=="prot")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o3<-i1

a3a<-filter(a3, column=="weight")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o4<-i1

a3a<-filter(a3, column=="dev")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o5<-i1

a3a<-filter(a3, column=="total")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o6<-i1

r1<-rbind(e1, e2,e3,e4,e5,e6)
t1<-rbind(o1,o2,o3,o4,o5,o6)

p1<-rbind(o1,o2,o3,o4,o5,o6)
x1.01<-sum(p1$`# sign p`)
x1.02<- sum(p1$`# sign fdrp`)
s1<-as.data.frame(cbind(x1.01,x1.02))
colnames(s1)[colnames(s1)=="x1.01"] <- "# sign p"
colnames(s1)[colnames(s1)=="x1.02"] <- "# sign fdrp"
s1$newcolumn<-paste(unique(w$DT))
colnames(s1)[colnames(s1)=="newcolumn"] <- "comparison"
d1<-s1

###
w<- q1[(q1$DT=="PHS13NS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample, X))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a2a<-a2[(a2$column=="total" | a2$column=="dev" | a2$column=="weight" | a2$column=="trig" | a2$column=="prot" | a2$column=="gluc"),]
a2<-a2a[!(a2a$row=="total" | a2a$row=="dev" | a2a$row=="weight" | a2a$row=="trig" | a2a$row=="prot" | a2a$row=="gluc"),]
a3<- drop_na(a2)

a3$newcolumn<- unique(w$Diet)
colnames(a3)[colnames(a3)=="newcolumn"] <- "Diet"
a3$newcolumn<- unique(w$"Treatment")
colnames(a3)[colnames(a3)=="newcolumn"] <- "Treatment"

a3a<-filter(a3, column=="trig")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o1<-i1

a3a<-filter(a3, column=="gluc")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o2<-i1

a3a<-filter(a3, column=="prot")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o3<-i1

a3a<-filter(a3, column=="weight")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o4<-i1

a3a<-filter(a3, column=="dev")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o5<-i1

a3a<-filter(a3, column=="total")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o6<-i1

r2<-rbind(e1, e2,e3,e4,e5,e6)
t2<-rbind(o1,o2,o3,o4,o5,o6)

p1<-rbind(o1,o2,o3,o4,o5,o6)
x1.01<-sum(p1$`# sign p`)
x1.02<- sum(p1$`# sign fdrp`)
s1<-as.data.frame(cbind(x1.01,x1.02))
colnames(s1)[colnames(s1)=="x1.01"] <- "# sign p"
colnames(s1)[colnames(s1)=="x1.02"] <- "# sign fdrp"
s1$newcolumn<-paste(unique(w$DT))
colnames(s1)[colnames(s1)=="newcolumn"] <- "comparison"
d2<-s1

###
w<- q1[(q1$DT=="PHSA13NS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample, X))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a2a<-a2[(a2$column=="total" | a2$column=="dev" | a2$column=="weight" | a2$column=="trig" | a2$column=="prot" | a2$column=="gluc"),]
a2<-a2a[!(a2a$row=="total" | a2a$row=="dev" | a2a$row=="weight" | a2a$row=="trig" | a2a$row=="prot" | a2a$row=="gluc"),]
a3<- drop_na(a2)

a3$newcolumn<- unique(w$Diet)
colnames(a3)[colnames(a3)=="newcolumn"] <- "Diet"
a3$newcolumn<- unique(w$"Treatment")
colnames(a3)[colnames(a3)=="newcolumn"] <- "Treatment"

a3a<-filter(a3, column=="trig")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o1<-i1

a3a<-filter(a3, column=="gluc")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o2<-i1

a3a<-filter(a3, column=="prot")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o3<-i1

a3a<-filter(a3, column=="weight")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o4<-i1

a3a<-filter(a3, column=="dev")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o5<-i1

a3a<-filter(a3, column=="total")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o6<-i1

r3<-rbind(e1, e2,e3,e4,e5,e6)
t3<-rbind(o1,o2,o3,o4,o5,o6)

p1<-rbind(o1,o2,o3,o4,o5,o6)
x1.01<-sum(p1$`# sign p`)
x1.02<- sum(p1$`# sign fdrp`)
s1<-as.data.frame(cbind(x1.01,x1.02))
colnames(s1)[colnames(s1)=="x1.01"] <- "# sign p"
colnames(s1)[colnames(s1)=="x1.02"] <- "# sign fdrp"
s1$newcolumn<-paste(unique(w$DT))
colnames(s1)[colnames(s1)=="newcolumn"] <- "comparison"
d3<-s1

###
w<- q1[(q1$DT=="PHSA6NS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample, X))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a2a<-a2[(a2$column=="total" | a2$column=="dev" | a2$column=="weight" | a2$column=="trig" | a2$column=="prot" | a2$column=="gluc"),]
a2<-a2a[!(a2a$row=="total" | a2a$row=="dev" | a2a$row=="weight" | a2a$row=="trig" | a2a$row=="prot" | a2a$row=="gluc"),]
a3<- drop_na(a2)

a3$newcolumn<- unique(w$Diet)
colnames(a3)[colnames(a3)=="newcolumn"] <- "Diet"
a3$newcolumn<- unique(w$"Treatment")
colnames(a3)[colnames(a3)=="newcolumn"] <- "Treatment"

a3a<-filter(a3, column=="trig")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o1<-i1

a3a<-filter(a3, column=="gluc")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o2<-i1

a3a<-filter(a3, column=="prot")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o3<-i1

a3a<-filter(a3, column=="weight")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o4<-i1

a3a<-filter(a3, column=="dev")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o5<-i1

a3a<-filter(a3, column=="total")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o6<-i1

r4<-rbind(e1, e2,e3,e4,e5,e6)
t4<-rbind(o1,o2,o3,o4,o5,o6)

p1<-rbind(o1,o2,o3,o4,o5,o6)
x1.01<-sum(p1$`# sign p`)
x1.02<- sum(p1$`# sign fdrp`)
s1<-as.data.frame(cbind(x1.01,x1.02))
colnames(s1)[colnames(s1)=="x1.01"] <- "# sign p"
colnames(s1)[colnames(s1)=="x1.02"] <- "# sign fdrp"
s1$newcolumn<-paste(unique(w$DT))
colnames(s1)[colnames(s1)=="newcolumn"] <- "comparison"
d4<-s1

### HS S

w<- q1[(q1$DT=="RHSS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample, X))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a2a<-a2[(a2$column=="total" | a2$column=="dev" | a2$column=="weight" | a2$column=="trig" | a2$column=="prot" | a2$column=="gluc"),]
a2<-a2a[!(a2a$row=="total" | a2a$row=="dev" | a2a$row=="weight" | a2a$row=="trig" | a2a$row=="prot" | a2a$row=="gluc"),]
a3<- drop_na(a2)

a3$newcolumn<- unique(w$Diet)
colnames(a3)[colnames(a3)=="newcolumn"] <- "Diet"
a3$newcolumn<- unique(w$"Treatment")
colnames(a3)[colnames(a3)=="newcolumn"] <- "Treatment"

a3a<-filter(a3, column=="trig")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o1<-i1

a3a<-filter(a3, column=="gluc")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o2<-i1

a3a<-filter(a3, column=="prot")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o3<-i1

a3a<-filter(a3, column=="weight")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o4<-i1

a3a<-filter(a3, column=="dev")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o5<-i1

a3a<-filter(a3, column=="total")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o6<-i1

r5<-rbind(e1, e2,e3,e4,e5,e6)
t5<-rbind(o1,o2,o3,o4,o5,o6)

p1<-rbind(o1,o2,o3,o4,o5,o6)
x1.01<-sum(p1$`# sign p`)
x1.02<- sum(p1$`# sign fdrp`)
s1<-as.data.frame(cbind(x1.01,x1.02))
colnames(s1)[colnames(s1)=="x1.01"] <- "# sign p"
colnames(s1)[colnames(s1)=="x1.02"] <- "# sign fdrp"
s1$newcolumn<-paste(unique(w$DT))
colnames(s1)[colnames(s1)=="newcolumn"] <- "comparison"
d5<-s1

###
w<- q1[(q1$DT=="PHS13S"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample, X))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a2a<-a2[(a2$column=="total" | a2$column=="dev" | a2$column=="weight" | a2$column=="trig" | a2$column=="prot" | a2$column=="gluc"),]
a2<-a2a[!(a2a$row=="total" | a2a$row=="dev" | a2a$row=="weight" | a2a$row=="trig" | a2a$row=="prot" | a2a$row=="gluc"),]
a3<- drop_na(a2)

a3$newcolumn<- unique(w$Diet)
colnames(a3)[colnames(a3)=="newcolumn"] <- "Diet"
a3$newcolumn<- unique(w$"Treatment")
colnames(a3)[colnames(a3)=="newcolumn"] <- "Treatment"

a3a<-filter(a3, column=="trig")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o1<-i1

a3a<-filter(a3, column=="gluc")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o2<-i1

a3a<-filter(a3, column=="prot")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o3<-i1

a3a<-filter(a3, column=="weight")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o4<-i1

a3a<-filter(a3, column=="dev")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o5<-i1

a3a<-filter(a3, column=="total")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o6<-i1

r6<-rbind(e1, e2,e3,e4,e5,e6)
t6<-rbind(o1,o2,o3,o4,o5,o6)

p1<-rbind(o1,o2,o3,o4,o5,o6)
x1.01<-sum(p1$`# sign p`)
x1.02<- sum(p1$`# sign fdrp`)
s1<-as.data.frame(cbind(x1.01,x1.02))
colnames(s1)[colnames(s1)=="x1.01"] <- "# sign p"
colnames(s1)[colnames(s1)=="x1.02"] <- "# sign fdrp"
s1$newcolumn<-paste(unique(w$DT))
colnames(s1)[colnames(s1)=="newcolumn"] <- "comparison"
d6<-s1

###

###


u2<- rbind(r1,r2,r3,r4,r5,r6)
u2$newcolumn<- "HS"
colnames(u2)[colnames(u2)=="newcolumn"] <- "Type"

f2<-rbind(t1,t2,t3,t4,t5,t6)
f2$newcolumn<- "HS"
colnames(f2)[colnames(f2)=="newcolumn"] <- "Type"

g2<- rbind(d1,d2,d3,d4,d5,d6)
g2$newcolumn<- "HS"
colnames(g2)[colnames(g2)=="newcolumn"] <- "Type"

# Controls N done only for experiment 2 

w<- q1[(q1$DT=="RNS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample, X))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a2a<-a2[(a2$column=="total" | a2$column=="dev" | a2$column=="weight" | a2$column=="trig" | a2$column=="prot" | a2$column=="gluc"),]
a2<-a2a[!(a2a$row=="total" | a2a$row=="dev" | a2a$row=="weight" | a2a$row=="trig" | a2a$row=="prot" | a2a$row=="gluc"),]
a3<- drop_na(a2)

a3$newcolumn<- unique(w$Diet)
colnames(a3)[colnames(a3)=="newcolumn"] <- "Diet"
a3$newcolumn<- unique(w$"Treatment")
colnames(a3)[colnames(a3)=="newcolumn"] <- "Treatment"

a3a<-filter(a3, column=="trig")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o1<-i1

a3a<-filter(a3, column=="gluc")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o2<-i1

a3a<-filter(a3, column=="prot")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o3<-i1

a3a<-filter(a3, column=="weight")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o4<-i1

a3a<-filter(a3, column=="dev")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o5<-i1

a3a<-filter(a3, column=="total")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o6<-i1

r1<-rbind(e1, e2,e3,e4,e5,e6)
t1<-rbind(o1,o2,o3,o4,o5,o6)

p1<-rbind(o1,o2,o3,o4,o5,o6)
x1.01<-sum(p1$`# sign p`)
x1.02<- sum(p1$`# sign fdrp`)
s1<-as.data.frame(cbind(x1.01,x1.02))
colnames(s1)[colnames(s1)=="x1.01"] <- "# sign p"
colnames(s1)[colnames(s1)=="x1.02"] <- "# sign fdrp"
s1$newcolumn<-paste(unique(w$DT))
colnames(s1)[colnames(s1)=="newcolumn"] <- "comparison"
d1<-s1

###
w<- q1[(q1$DT=="PRNS"),]
a<- select(w, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample, X))
a1<- rcorr(as.matrix(a), type= "spearman")
a2<-flattenCorrMatrix(a1$r, a1$P)
a2a<-a2[(a2$column=="total" | a2$column=="dev" | a2$column=="weight" | a2$column=="trig" | a2$column=="prot" | a2$column=="gluc"),]
a2<-a2a[!(a2a$row=="total" | a2a$row=="dev" | a2a$row=="weight" | a2a$row=="trig" | a2a$row=="prot" | a2a$row=="gluc"),]
a3<- drop_na(a2)

a3$newcolumn<- unique(w$Diet)
colnames(a3)[colnames(a3)=="newcolumn"] <- "Diet"
a3$newcolumn<- unique(w$"Treatment")
colnames(a3)[colnames(a3)=="newcolumn"] <- "Treatment"

a3a<-filter(a3, column=="trig")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o1<-i1

a3a<-filter(a3, column=="gluc")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o2<-i1

a3a<-filter(a3, column=="prot")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o3<-i1

a3a<-filter(a3, column=="weight")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o4<-i1

a3a<-filter(a3, column=="dev")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o5<-i1

a3a<-filter(a3, column=="total")
a4<- adjust_pvalue(a3a, p.col= "p", output.col= "bonferroni p", method= "bonferroni")
a5<- adjust_pvalue(a4, p.col= "p", output.col= "fdr p", method= "fdr")
a5<- a5[order(a5$"p"),]
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
i1$newcolumn<-paste(unique(a3a$column))
colnames(i1)[colnames(i1)=="newcolumn"] <- "phenotype"
o6<-i1

r2<-rbind(e1, e2,e3,e4,e5,e6)
t2<-rbind(o1,o2,o3,o4,o5,o6)

p1<-rbind(o1,o2,o3,o4,o5,o6)
x1.01<-sum(p1$`# sign p`)
x1.02<- sum(p1$`# sign fdrp`)
s1<-as.data.frame(cbind(x1.01,x1.02))
colnames(s1)[colnames(s1)=="x1.01"] <- "# sign p"
colnames(s1)[colnames(s1)=="x1.02"] <- "# sign fdrp"
s1$newcolumn<-paste(unique(w$DT))
colnames(s1)[colnames(s1)=="newcolumn"] <- "comparison"
d2<-s1

u3<- rbind(r1,r2)
u3$newcolumn<- "N"
colnames(u3)[colnames(u3)=="newcolumn"] <- "Type"

f3<-rbind(t1,t2)
f3$newcolumn<- "N"
colnames(f3)[colnames(f3)=="newcolumn"] <- "Type"

g3<- rbind(d1,d2)
g3$newcolumn<- "N"
colnames(g3)[colnames(g3)=="newcolumn"] <- "Type"

u4<- rbind(u1,u2,u3)
colnames(u4)[colnames(u4)=="row"] <- "Taxa"
colnames(u4)[colnames(u4)=="column"] <- "Phenotype"
u4$Phenotype<- as.character(u4$Phenotype)
u4$Phenotype[u4$Phenotype=="trig"]<-"Triglyceride"
u4$Phenotype[u4$Phenotype=="prot"]<-"Protein"
u4$Phenotype[u4$Phenotype=="gluc"]<-"Glucose"
u4$Phenotype[u4$Phenotype=="dev"]<-"Development"
u4$Phenotype[u4$Phenotype=="total"]<-"Total"
u4$Phenotype[u4$Phenotype=="weight"]<-"Weight"
u4$Diet <- gsub("13", "11", u4$Diet)

f4<-rbind(f1,f2,f3)
f4$phenotype<- as.character(f4$phenotype)
f4$phenotype[f4$phenotype=="trig"]<-"Triglyceride"
f4$phenotype[f4$phenotype=="prot"]<-"Protein"
f4$phenotype[f4$phenotype=="gluc"]<-"Glucose"
f4$phenotype[f4$phenotype=="dev"]<-"Development"
f4$phenotype[f4$phenotype=="total"]<-"Total"
f4$phenotype[f4$phenotype=="weight"]<-"Weight"
f4$comparison <- gsub("13", "11", f4$comparison)

g4<- rbind(g1,g2,g3)
g4$comparison <- gsub("13", "11", g4$comparison)

write.csv(u4, "sp_z_n_round_phen_d_t_z.csv")
write.csv(f4, "#sign_sp_z_per_round_phen_d_t_z.csv")
write.csv(g4, "#sign_sp_z_all_round_phen_d_t_z.csv")
