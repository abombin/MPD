library(dplyr)
library(vegan)

q<-as.data.frame(hfhs_wunif_all)
DT<-as.data.frame(paste(q$Diet, q$Treatment, sep=""))
q1<-cbind(DT, q)
colnames(q1)[1]<-"DT"

### HS
b<- q1[(q1$"DT"=="RHSNS" | q1$"DT"=="RHSS"),]
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
i1<-t

b<- q1[(q1$"DT"=="PHS13NS" | q1$"DT"=="PHS13S"),]
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
i2<-t

b<- q1[(q1$"DT"=="PHSA13NS" | q1$"DT"=="PHSA13S"),]
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
i3<-t

b<- q1[(q1$"DT"=="PHSA6NS" | q1$"DT"=="PHSA6S"),]
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
i4<-t

p1<-rbind(i1,i2,i3,i4)

p1$newcolumn<-"HS"
colnames(p1)[colnames(p1)=="newcolumn"] <- "Type"

### HF
b<- q1[(q1$"DT"=="RHFNS" | q1$"DT"=="RHFS"),]
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
i1<-t

b<- q1[(q1$"DT"=="PHFNS" | q1$"DT"=="PHFS"),]
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
i2<-t

b<- q1[(q1$"DT"=="PHFANS" | q1$"DT"=="PHFAS"),]
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
i3<-t

p2<-rbind(i1,i2,i3,i4)

p2$newcolumn<-"HF"
colnames(p2)[colnames(p2)=="newcolumn"] <- "Type"

o1<-rbind(p1,p2)

o1$comparison <- gsub("13", "11", o1$comparison)

write.csv(o1, "Anosim_hfhs_zn_wunif_t_all.csv")