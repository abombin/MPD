library(Hmisc)
library(xlsx)
library(rstatix)
library(dplyr)
library(matrixTests)
library(data.table)
q<- hfhs_trans_zotu_normalized_sum_genera
a<-q
# HF comparisons
diet1<- a[(a$DT=="RHFNS"),]
diet2<- a[(a$DT=="RHFS"),]
x <- select(diet1, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
y <- select(diet2, -c(Label, Group, "Genetic line", Treatment, Round,  "Label 2", Diet, DT, Type, Sample))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"RHF"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
e1<- w_p6

i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$Diet)
i1$newcolumn<-paste(s1)
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o1<-i1

diet1<- a[(a$DT=="PHFNS"),]
diet2<- a[(a$DT=="PHFS"),]
x <- select(diet1, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
y <- select(diet2, -c(Label, Group, "Genetic line", Treatment, Round,  "Label 2", Diet, DT, Type, Sample))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHF"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
e2<- w_p6

i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$Diet)
i1$newcolumn<-paste(s1)
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o2<-i1

diet1<- a[(a$DT=="PHFANS"),]
diet2<- a[(a$DT=="PHFAS"),]
x <- select(diet1, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
y <- select(diet2, -c(Label, Group, "Genetic line", Treatment, Round,  "Label 2", Diet, DT, Type, Sample))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHFA"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
e3<- w_p6

i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$Diet)
i1$newcolumn<-paste(s1)
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o3<-i1

t1<- rbind(e1,e2,e3)
t1$newcolumn<-"HF"
colnames(t1)[colnames(t1)=="newcolumn"] <- "Type"

# HS comparisons
diet1<- a[(a$DT=="RHSNS"),]
diet2<- a[(a$DT=="RHSS"),]
x <- select(diet1, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
y <- select(diet2, -c(Label, Group, "Genetic line", Treatment, Round,  "Label 2", Diet, DT, Type, Sample))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"RHS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
e1<- w_p6

i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$Diet)
i1$newcolumn<-paste(s1)
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o4<-i1

diet1<- a[(a$DT=="PHS13NS"),]
diet2<- a[(a$DT=="PHS13S"),]
x <- select(diet1, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
y <- select(diet2, -c(Label, Group, "Genetic line", Treatment, Round,  "Label 2", Diet, DT, Type, Sample))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHS11"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
e2<- w_p6

i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$Diet)
i1$newcolumn<-paste(s1)
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o5<-i1

diet1<- a[(a$DT=="PHSA13NS"),]
diet2<- a[(a$DT=="PHSA13S"),]
x <- select(diet1, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
y <- select(diet2, -c(Label, Group, "Genetic line", Treatment, Round,  "Label 2", Diet, DT, Type, Sample))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHSA11"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
e3<- w_p6

i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$Diet)
i1$newcolumn<-paste(s1)
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o6<-i1

diet1<- a[(a$DT=="PHSA6NS"),]
diet2<- a[(a$DT=="PHSA6S"),]
x <- select(diet1, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
y <- select(diet2, -c(Label, Group, "Genetic line", Treatment, Round,  "Label 2", Diet, DT, Type, Sample))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHSA6"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
e4<- w_p6

i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$Diet)
i1$newcolumn<-paste(s1)
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o7<-i1

t2<- rbind(e1,e2,e3,e4)
t2$newcolumn<-"HS"
colnames(t2)[colnames(t2)=="newcolumn"] <- "Type"
u1<- rbind(t1,t2)
colnames(u1)[colnames(u1)=="rn"] <- "ZOTU ID"
u1$Diet <- sub("13", "11", u1$Diet)
write.csv(u1, "Wilcox_hfhs_z_g_norm_t.csv")
p<-rbind(o1,o2,o3,o4,o5,o6,o7)
p$comparison <- sub("13", "11", p$comparison)
write.csv(p, "Wilcox_hfhs_z_g_norm_#sign_t.csv")
