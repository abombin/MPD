library(dplyr)
library(vegan)

y<- transpose_100_zotu_unmod_norm
q<-y
# Controls
b<- q[(q$"DT"=="RNS" | q$"DT"=="PRNS"),]
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
i1$newcolumn<-"N"
colnames(i1)[colnames(i1)=="newcolumn"] <- "Type"

b<- q[(q$"DT"=="RHSNS" | q$"DT"=="RNS"),]
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
u2<-t

b<- q[(q$"DT"=="PHS13NS" | q$"DT"=="PRNS"),]
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
u3<-t

i2<-rbind(u2,u3)
i2$newcolumn<-"N vs HS"
colnames(i2)[colnames(i2)=="newcolumn"] <- "Type"

b<- q[(q$"DT"=="RHFNS" | q$"DT"=="RNS"),]
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
u4<-t

b<- q[(q$"DT"=="PHFNS" | q$"DT"=="PRNS"),]
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
u5<-t

i3<-rbind(u4,u5)
i3$newcolumn<-"N vs HF"
colnames(i3)[colnames(i3)=="newcolumn"] <- "Type"

# HF 

b<- q[(q$"DT"=="RHFNS" | q$"DT"=="PHFNS"),]
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
u1<-t

b<- q[(q$"DT"=="RHFNS" | q$"DT"=="PHFANS"),]
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
u2<-t

b<- q[(q$"DT"=="PHFNS" | q$"DT"=="PHFANS"),]
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
u3<-t

b<- q[(q$"DT"=="RHFS" | q$"DT"=="PHFS"),]
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
u4<-t

b<- q[(q$"DT"=="RHFS" | q$"DT"=="PHFAS"),]
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
u5<-t

b<- q[(q$"DT"=="PHFS" | q$"DT"=="PHFAS"),]
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
u6<-t

i4<- rbind(u1,u2,u3,u4,u5,u6)
i4$newcolumn<-"HF"
colnames(i4)[colnames(i4)=="newcolumn"] <- "Type"

#HS 

b<- q[(q$"DT"=="RHSNS" | q$"DT"=="PHS13NS"),]
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
u1<-t

b<- q[(q$"DT"=="RHSNS" | q$"DT"=="PHSA13NS"),]
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
u2<-t

b<- q[(q$"DT"=="RHSNS" | q$"DT"=="PHSA6NS"),]
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
u3<-t

b<- q[(q$"DT"=="PHS13NS" | q$"DT"=="PHSA13NS"),]
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
u4<-t

b<- q[(q$"DT"=="PHS13NS" | q$"DT"=="PHSA6NS"),]
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
u5<-t

b<- q[(q$"DT"=="PHSA13NS" | q$"DT"=="PHSA6NS"),]
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
u6<-t

#S
b<- q[(q$"DT"=="RHSS" | q$"DT"=="PHS13S"),]
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
u7<-t

b<- q[(q$"DT"=="RHSS" | q$"DT"=="PHSA13S"),]
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
u8<-t

b<- q[(q$"DT"=="RHSS" | q$"DT"=="PHSA6S"),]
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
u9<-t

b<- q[(q$"DT"=="PHS13S" | q$"DT"=="PHSA13S"),]
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
u10<-t

b<- q[(q$"DT"=="PHS13S" | q$"DT"=="PHSA6S"),]
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
u11<-t

b<- q[(q$"DT"=="PHSA13S" | q$"DT"=="PHSA6S"),]
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
u12<-t

i5<-rbind(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12)

i5$newcolumn<-"HS"
colnames(i5)[colnames(i5)=="newcolumn"] <- "Type"

o1<-rbind(i1,i2,i3,i4,i5)

o1$comparison <- gsub("13", "11", o1$comparison)

write.csv(o1, "Anosim_hfhs_zotu100_norm_bray_d.csv")