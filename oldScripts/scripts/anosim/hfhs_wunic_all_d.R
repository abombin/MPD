library(dplyr)
library(vegan)

q<-as.data.frame(hfhs_wunif_all)
DT<-as.data.frame(paste(q$Diet, q$Treatment, sep=""))
q1<-cbind(DT, q)
colnames(q1)[1]<-"DT"
# HF NS
b<- q1[(q1$DT=="RHFNS"|q1$DT=="PHFNS"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u1<-t

b<- q1[(q1$DT=="RHFNS"|q1$DT=="PHFANS"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u2<-t

b<- q1[(q1$"DT"=="PHFNS" | q1$"DT"=="PHFANS"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u3<-t

### HF S
b<- q1[(q1$"DT"=="RHFS" | q1$"DT"=="PHFS"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u4<-t

b<- q1[(q1$"DT"=="RHFS" | q1$"DT"=="PHFAS"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u5<-t

b<- q1[(q1$"DT"=="PHFS" | q1$"DT"=="PHFAS"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u6<-t

i4<- rbind(u1,u2,u3,u4,u5,u6)
i4$newcolumn<-"HF"
colnames(i4)[colnames(i4)=="newcolumn"] <- "Type"

### HS NS
b<- q1[(q1$"DT"=="RHSNS" | q1$"DT"=="PHS13NS"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u1<-t

b<- q1[(q1$"DT"=="RHSNS" | q1$"DT"=="PHSA13NS"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u2<-t

b<- q1[(q1$"DT"=="RHSNS" | q1$"DT"=="PHSA6NS"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u3<-t

b<- q1[(q1$"DT"=="PHS13NS" | q1$"DT"=="PHSA13NS"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u4<-t

b<- q1[(q1$"DT"=="PHS13NS" | q1$"DT"=="PHSA6NS"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u5<-t

b<- q1[(q1$"DT"=="PHSA13NS" | q1$"DT"=="PHSA6NS"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u6<-t

### HS S

b<- q1[(q1$"DT"=="RHSS" | q1$"DT"=="PHS13S"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u7<-t

b<- q1[(q1$"DT"=="RHSS" | q1$"DT"=="PHSA13S"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u8<-t

b<- q1[(q1$"DT"=="RHSS" | q1$"DT"=="PHSA6S"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u9<-t

b<- q1[(q1$"DT"=="PHS13S" | q1$"DT"=="PHSA13S"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u10<-t

b<- q1[(q1$"DT"=="PHS13S" | q1$"DT"=="PHSA6S"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u11<-t

b<- q1[(q1$"DT"=="PHSA13S" | q1$"DT"=="PHSA6S"),]
a<-as.vector(b$`Column 1`)
w<- as.data.frame(b[, a])
e1 <- as.vector(b$"DT")
r<- anosim(w, e1, permutations = 999)
t<-as.data.frame(r$signif)
s1<-as.data.frame(unique(b$DT))
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
u12<-t

i5<-rbind(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12)

i5$newcolumn<-"HS"
colnames(i5)[colnames(i5)=="newcolumn"] <- "Type"

o1<-rbind(i4,i5)

o1$comparison <- gsub("13", "11", o1$comparison)

write.csv(o1, "Anosim_hfhs_zn_wunif_d_all.csv")
