library(Hmisc)
library(xlsx)
library(rstatix)
library(dplyr)
library(matrixTests)
library(data.table)

q<- all_d_zn_z
q$DT<- paste(q$Diet, q$Treatment, sep="")
a<-q

# HF vs N NS
diet1<- a[(a$DT=="RHFNS"),]
diet2<- a[(a$DT=="RNS"),]
x <- select(diet1, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
y <- select(diet2, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"RHF vs R"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e1<- w_p6

i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$DT)
s2<-unique(diet2$DT)
i1$newcolumn<-paste(s1, s2, sep=" vs ")
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o1<-i1


diet1<- a[(a$DT=="PHFNS"),]
diet2<- a[(a$DT=="PRNS"),]
x <- select(diet1, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
y <- select(diet2, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHF vs PR"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e2<-w_p6
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$DT)
s2<-unique(diet2$DT)
i1$newcolumn<-paste(s1, s2, sep=" vs ")
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o2<-i1

diet1<- a[(a$DT=="PHFANS"),]
diet2<- a[(a$DT=="PANS"),]
x <- select(diet1, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
y <- select(diet2, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHFA vs PA"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e3<-w_p6
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$DT)
s2<-unique(diet2$DT)
i1$newcolumn<-paste(s1, s2, sep=" vs ")
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o3<-i1

r1<-rbind(e1,e2,e3)

# HF comparisons S

diet1<- a[(a$DT=="RHFS"),]
diet2<- a[(a$DT=="RS"),]
x <- select(diet1, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
y <- select(diet2, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"RHF vs R"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e1<- w_p6
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$DT)
s2<-unique(diet2$DT)
i1$newcolumn<-paste(s1, s2, sep=" vs ")
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o4<-i1

diet1<- a[(a$DT=="PHFS"),]
diet2<- a[(a$DT=="PRS"),]
x <- select(diet1, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
y <- select(diet2, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHF vs PR"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e2<-w_p6
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$DT)
s2<-unique(diet2$DT)
i1$newcolumn<-paste(s1, s2, sep=" vs ")
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o5<-i1

diet1<- a[(a$DT=="PHFAS"),]
diet2<- a[(a$DT=="PAS"),]
x <- select(diet1, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
y <- select(diet2, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHFA vs PA"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e3<-w_p6
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$DT)
s2<-unique(diet2$DT)
i1$newcolumn<-paste(s1, s2, sep=" vs ")
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o6<-i1

r2<-rbind(e1,e2,e3)
t1<-rbind(r1,r2)
t1$newcolumn<-"HF vs N"
colnames(t1)[colnames(t1)=="newcolumn"] <- "Type"

# HS Comparisons NS

diet1<- a[(a$DT=="RHSNS"),]
diet2<- a[(a$DT=="RNS"),]
x <- select(diet1, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
y <- select(diet2, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"RHS vs R"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e1<- w_p6
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$DT)
s2<-unique(diet2$DT)
i1$newcolumn<-paste(s1, s2, sep=" vs ")
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o7<-i1

diet1<- a[(a$DT=="PHS13NS"),]
diet2<- a[(a$DT=="PRNS"),]
x <- select(diet1, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
y <- select(diet2, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHS13 vs PR"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e2<- w_p6
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$DT)
s2<-unique(diet2$DT)
i1$newcolumn<-paste(s1, s2, sep=" vs ")
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o8<-i1

diet1<- a[(a$DT=="PHSA6NS"),]
diet2<- a[(a$DT=="PANS"),]
x <- select(diet1, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
y <- select(diet2, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHSA6 vs PA"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e3<- w_p6
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$DT)
s2<-unique(diet2$DT)
i1$newcolumn<-paste(s1, s2, sep=" vs ")
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o9<-i1

diet1<- a[(a$DT=="PHSA13NS"),]
diet2<- a[(a$DT=="PANS"),]
x <- select(diet1, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
y <- select(diet2, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHSA11 vs PA"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e4<- w_p6
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$DT)
s2<-unique(diet2$DT)
i1$newcolumn<-paste(s1, s2, sep=" vs ")
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o10<-i1

r3<-rbind(e1,e2,e3,e4)

# HS Comparison S

diet1<- a[(a$DT=="RHSS"),]
diet2<- a[(a$DT=="RS"),]
x <- select(diet1, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
y <- select(diet2, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"RHS vs R"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e1<- w_p6
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$DT)
s2<-unique(diet2$DT)
i1$newcolumn<-paste(s1, s2, sep=" vs ")
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o13<-i1


diet1<- a[(a$DT=="PHS13S"),]
diet2<- a[(a$DT=="PRS"),]
x <- select(diet1, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
y <- select(diet2, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHS vs PR"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e2<- w_p6
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$DT)
s2<-unique(diet2$DT)
i1$newcolumn<-paste(s1, s2, sep=" vs ")
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o14<-i1

diet1<- a[(a$DT=="PHSA6S"),]
diet2<- a[(a$DT=="PAS"),]
x <- select(diet1, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
y <- select(diet2, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHSA6 vs PA"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e3<- w_p6
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$DT)
s2<-unique(diet2$DT)
i1$newcolumn<-paste(s1, s2, sep=" vs ")
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o15<-i1

diet1<- a[(a$DT=="PHSA13S"),]
diet2<- a[(a$DT=="PAS"),]
x <- select(diet1, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
y <- select(diet2, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
w<- col_wilcoxon_twosample(x, y, alternative = "two.sided", mu = 0, exact = NA, correct = TRUE)
w1<- setDT(w, keep.rownames = TRUE)[]
w_p<- adjust_pvalue(w1, p.col= "pvalue", output.col= "bonferroni p", method= "bonferroni")
w_p1<- adjust_pvalue(w_p, p.col= "pvalue", output.col= "fdr p", method= "fdr")
w_p6<- drop_na(w_p1)
w_p6<- w_p6[order(w_p6$"pvalue"),]
x1.01<- w_p6[w_p6$p < 0.05, ]
x1.02<- w_p6[w_p6$"fdr p" < 0.1, ]
w_p6$newcolumn<-"PHSA11 vs PA"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e4<- w_p6
i<-nrow(x1.01)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"
s1<-unique(diet1$DT)
s2<-unique(diet2$DT)
i1$newcolumn<-paste(s1, s2, sep=" vs ")
colnames(i1)[colnames(i1)=="newcolumn"] <- "comparison"
o16<-i1



r4<-rbind(e1,e2,e3,e4)
t2<-rbind(r3,r4)
t2$newcolumn<-"HS vs N"
colnames(t2)[colnames(t2)=="newcolumn"] <- "Type"


u1<-rbind(t1,t2)
colnames(u1)[colnames(u1)=="rn"] <- "Taxa"
u1$Diet <- gsub("13", "11", u1$Diet)
u1$Taxa <- gsub('X.', '', u1$Taxa)
write.csv(u1, "Wilcox_all_d_zn_z.csv")

p<-rbind(o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o13,o14,o15,o16)

p$comparison <- gsub("13", "11", p$comparison)

write.csv(p, "Wilcox_#sign_all_d_zn_z.csv")