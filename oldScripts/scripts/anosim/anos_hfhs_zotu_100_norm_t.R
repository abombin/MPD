library(dplyr)
library(vegan)

y<- transpose_100_zotu_unmod_norm
q<-y
#HS
b<- q[(q$"DT"=="RHSNS" | q$"DT"=="RHSS"),]
w<- select(b, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
e1 <- as.vector(b$"DT") 
r<- anosim(w, e1, permutations = 999, distance = "bray")
t<-as.data.frame(r$signif)
s1<-unique(b$DT)
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
i1<-t


b<- q[(q$"DT"=="PHS13NS" | q$"DT"=="PHS13S"),]
w<- select(b, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
e1 <- as.vector(b$"DT") 
r<- anosim(w, e1, permutations = 999, distance = "bray")
t<-as.data.frame(r$signif)
s1<-unique(b$DT)
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
i2<-t

b<- q[(q$"DT"=="PHSA13NS" | q$"DT"=="PHSA13S"),]
w<- select(b, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
e1 <- as.vector(b$"DT") 
r<- anosim(w, e1, permutations = 999, distance = "bray")
t<-as.data.frame(r$signif)
s1<-unique(b$DT)
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
i3<-t

b<- q[(q$"DT"=="PHSA6NS" | q$"DT"=="PHSA6S"),]
w<- select(b, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
e1 <- as.vector(b$"DT") 
r<- anosim(w, e1, permutations = 999, distance = "bray")
t<-as.data.frame(r$signif)
s1<-unique(b$DT)
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
i4<-t

p1<-rbind(i1,i2,i3,i4)

p1$newcolumn<-"HS"
colnames(p1)[colnames(p1)=="newcolumn"] <- "Type"

#HF

b<- q[(q$"DT"=="RHFNS" | q$"DT"=="RHFS"),]
w<- select(b, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
e1 <- as.vector(b$"DT") 
r<- anosim(w, e1, permutations = 999, distance = "bray")
t<-as.data.frame(r$signif)
s1<-unique(b$DT)
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
i1<-t

b<- q[(q$"DT"=="PHFNS" | q$"DT"=="PHFS"),]
w<- select(b, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
e1 <- as.vector(b$"DT") 
r<- anosim(w, e1, permutations = 999, distance = "bray")
t<-as.data.frame(r$signif)
s1<-unique(b$DT)
s2<-as.data.frame(t(s1))
s3<-paste(s2$V1, s2$V2, sep=" vs ")
t$newcolumn<-paste(s3)
colnames(t)[colnames(t)=="newcolumn"] <- "comparison"
colnames(t)[colnames(t)=="r$signif"] <- "p value"
i2<-t

b<- q[(q$"DT"=="PHFANS" | q$"DT"=="PHFAS"),]
w<- select(b, -c(Label, Group, Diet, "Genetic line", Treatment, Round, "Label 2", Sample, "DT", Type))
e1 <- as.vector(b$"DT") 
r<- anosim(w, e1, permutations = 999, distance = "bray")
t<-as.data.frame(r$signif)
s1<-unique(b$DT)
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

write.csv(o1, "Anosim_hfhs_zotu100_norm_bray_t.csv")