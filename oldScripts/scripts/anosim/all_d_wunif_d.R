library(dplyr)
library(vegan)

q<-as.data.frame(q5)
DT<-as.data.frame(paste(q5$Diet, q5$Treatment, sep=""))
q1<-cbind(DT, q)
colnames(q1)[1]<-"DT"
# N vs HF
b<- q1[(q1$"DT"=="RNS" | q1$"DT"=="RHFNS"),]
a<-as.vector(b$X1)
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

b<- q1[(q1$"DT"=="PRNS" | q1$"DT"=="PHFNS"),]
a<-as.vector(b$X1)
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

b<- q1[(q1$"DT"=="PANS" | q1$"DT"=="PHFANS"),]
a<-as.vector(b$X1)
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

###  S
b<- q1[(q1$"DT"=="RS" | q1$"DT"=="RHFS"),]
a<-as.vector(b$X1)
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

b<- q1[(q1$"DT"=="PRS" | q1$"DT"=="PHFS"),]
a<-as.vector(b$X1)
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

b<- q1[(q1$"DT"=="PAS" | q1$"DT"=="PHFAS"),]
a<-as.vector(b$X1)
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
i4$newcolumn<-"N vs HF"
colnames(i4)[colnames(i4)=="newcolumn"] <- "Type"

### HS NS
b<- q1[(q1$"DT"=="RNS" | q1$"DT"=="RHSNS"),]
a<-as.vector(b$X1)
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

b<- q1[(q1$"DT"=="PRNS" | q1$"DT"=="PHS13NS"),]
a<-as.vector(b$X1)
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

b<- q1[(q1$"DT"=="PANS" | q1$"DT"=="PHSA6NS"),]
a<-as.vector(b$X1)
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

b<- q1[(q1$"DT"=="PANS" | q1$"DT"=="PHSA13NS"),]
a<-as.vector(b$X1)
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

### S
b<- q1[(q1$"DT"=="RS" | q1$"DT"=="RHSS"),]
a<-as.vector(b$X1)
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

b<- q1[(q1$"DT"=="PRS" | q1$"DT"=="PHS13S"),]
a<-as.vector(b$X1)
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


b<- q1[(q1$"DT"=="PAS" | q1$"DT"=="PHSA6S"),]
a<-as.vector(b$X1)
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

b<- q1[(q1$"DT"=="PAS" | q1$"DT"=="PHSA13S"),]
a<-as.vector(b$X1)
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

i5<-rbind(u1,u2,u3,u4,u5,u6,u7,u8)
i5$newcolumn<-"N vs HS"
colnames(i5)[colnames(i5)=="newcolumn"] <- "Type"

o1<-rbind(i4,i5)

o1$comparison <- gsub("13", "11", o1$comparison)

write.csv(o1, "Anosim_all_d_zn_wunif_d.csv")