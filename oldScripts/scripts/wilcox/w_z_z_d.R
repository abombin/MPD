library(Hmisc)
library(xlsx)
library(rstatix)
library(dplyr)
library(matrixTests)
library(data.table)
q<- transpose_all_zotu_norm
a<-q
# HF comparisons NS
diet1<- a[(a$DT=="RHFNS"),]
diet2<- a[(a$DT=="PHFNS"),]
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
w_p6$newcolumn<-"RHFvsPHF"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e1<- w_p6


diet1<- a[(a$DT=="RHFNS"),]
diet2<- a[(a$DT=="PHFANS"),]
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
w_p6$newcolumn<-"RHFvsPHFA"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e2<-w_p6

diet1<- a[(a$DT=="PHFNS"),]
diet2<- a[(a$DT=="PHFANS"),]
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
w_p6$newcolumn<-"PHFvsPHFA"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e3<-w_p6

r1<-rbind(e1,e2,e3)

# HF comparisons S

diet1<- a[(a$DT=="RHFS"),]
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
w_p6$newcolumn<-"RHFvsPHF"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e1<- w_p6


diet1<- a[(a$DT=="RHFS"),]
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
w_p6$newcolumn<-"RHFvsPHFA"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e2<-w_p6

diet1<- a[(a$DT=="PHFS"),]
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
w_p6$newcolumn<-"PHFvsPHFA"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e3<-w_p6

r2<-rbind(e1,e2,e3)
t1<-rbind(r1,r2)
t1$newcolumn<-"HF"
colnames(t1)[colnames(t1)=="newcolumn"] <- "Type"

# HS Comparisons NS

diet1<- a[(a$DT=="RHSNS"),]
diet2<- a[(a$DT=="PHS13NS"),]
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
w_p6$newcolumn<-"RHSvsPHS11"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e1<- w_p6

diet1<- a[(a$DT=="RHSNS"),]
diet2<- a[(a$DT=="PHSA13NS"),]
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
w_p6$newcolumn<-"RHSvsPHSA11"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e2<- w_p6

diet1<- a[(a$DT=="RHSNS"),]
diet2<- a[(a$DT=="PHSA6NS"),]
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
w_p6$newcolumn<-"RHSvsPHSA6"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e3<- w_p6

diet1<- a[(a$DT=="PHS13NS"),]
diet2<- a[(a$DT=="PHSA13NS"),]
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
w_p6$newcolumn<-"PHS11vsPHSA11"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e4<- w_p6

diet1<- a[(a$DT=="PHS13NS"),]
diet2<- a[(a$DT=="PHSA6NS"),]
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
w_p6$newcolumn<-"PHS11vsPHSA6"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e5<- w_p6

diet1<- a[(a$DT=="PHSA13NS"),]
diet2<- a[(a$DT=="PHSA6NS"),]
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
w_p6$newcolumn<-"PHSA11vsPHSA6"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e6<- w_p6

r3<-rbind(e1,e2,e3,e4,e5,e6)

# HS Comparison S

diet1<- a[(a$DT=="RHSS"),]
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
w_p6$newcolumn<-"RHSvsPHS11"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e1<- w_p6

diet1<- a[(a$DT=="RHSS"),]
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
w_p6$newcolumn<-"RHSvsPHSA11"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e2<- w_p6

diet1<- a[(a$DT=="RHSS"),]
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
w_p6$newcolumn<-"RHSvsPHSA6"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e3<- w_p6

diet1<- a[(a$DT=="PHS13S"),]
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
w_p6$newcolumn<-"PHS11vsPHSA11"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e4<- w_p6

diet1<- a[(a$DT=="PHS13S"),]
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
w_p6$newcolumn<-"PHS11vsPHSA6"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e5<- w_p6

diet1<- a[(a$DT=="PHSA13S"),]
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
w_p6$newcolumn<-"PHSA11vsPHSA6"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"S"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e6<- w_p6

r4<-rbind(e1,e2,e3,e4,e5,e6)
t2<-rbind(r3,r4)
t2$newcolumn<-"HS"
colnames(t2)[colnames(t2)=="newcolumn"] <- "Type"
# Comparing with controls

diet1<- a[(a$DT=="RHSNS"),]
diet2<- a[(a$DT=="RNS"),]
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
w_p6$newcolumn<-"RHSvsR"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e1<- w_p6

diet1<- a[(a$DT=="PHS13NS"),]
diet2<- a[(a$DT=="PRNS"),]
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
w_p6$newcolumn<-"PHS11vsPR"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e2<- w_p6
t3<-rbind(e1,e2)
t3$newcolumn<-"HSvsN"
colnames(t3)[colnames(t3)=="newcolumn"] <- "Type"


diet1<- a[(a$DT=="RHFNS"),]
diet2<- a[(a$DT=="RNS"),]
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
w_p6$newcolumn<-"RHFvsR"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e1<- w_p6

diet1<- a[(a$DT=="PHFNS"),]
diet2<- a[(a$DT=="PRNS"),]
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
w_p6$newcolumn<-"PHFvsPR"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Diet"
w_p6$newcolumn<-"NS"
colnames(w_p6)[colnames(w_p6)=="newcolumn"] <- "Treatment"
e2<- w_p6

t4<-rbind(e1,e2)
t4$newcolumn<-"HFvsN"
colnames(t4)[colnames(t4)=="newcolumn"] <- "Type"

u1<-rbind(t1,t2,t3,t4)
colnames(u1)[colnames(u1)=="rn"] <- "ZOTU ID"

write.csv(u1, "Wilcox_hfhs_z_z_norm_d.csv")
