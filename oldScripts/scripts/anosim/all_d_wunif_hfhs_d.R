library(dplyr)
library(vegan)

q<-as.data.frame(all_d_wunif_5gen)
DT<-as.data.frame(paste(q$Diet, q$Treatment, sep=""))
q1<-cbind(DT, q)
colnames(q1)[1]<-"DT"

b<- q1[(q1$"DT"=="RHFNS" | q1$"DT"=="RHSNS"),]
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

b<- q1[(q1$"DT"=="PHFNS" | q1$"DT"=="PHS13NS"),]
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

b<- q1[(q1$"DT"=="PHFANS" | q1$"DT"=="PHSA6NS"),]
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

b<- q1[(q1$"DT"=="PHFANS" | q1$"DT"=="PHSA13NS"),]
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
b<- q1[(q1$"DT"=="RHFS" | q1$"DT"=="RHSS"),]
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

b<- q1[(q1$"DT"=="PHFS" | q1$"DT"=="PHS13S"),]
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


b<- q1[(q1$"DT"=="PHFAS" | q1$"DT"=="PHSA6S"),]
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

b<- q1[(q1$"DT"=="PHFAS" | q1$"DT"=="PHSA13S"),]
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


i5$comparison <- gsub("13", "11", i5$comparison)

write.csv(i5, "Anosim_all_d_zn_wunif_hfhs_d.csv")