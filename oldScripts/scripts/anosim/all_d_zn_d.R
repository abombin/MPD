library(dplyr)
library(vegan)

y<- all_d_zn_g
q<-y
q$DT<- paste(q$Diet, q$Treatment, sep="")
# N vs HF
b<- q[(q$"DT"=="RNS" | q$"DT"=="RHFNS"),]
w<- select(b, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
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

b<- q[(q$"DT"=="PRNS" | q$"DT"=="PHFNS"),]
w<- select(b, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
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

b<- q[(q$"DT"=="PANS" | q$"DT"=="PHFANS"),]
w<- select(b, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
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

# S
b<- q[(q$"DT"=="RS" | q$"DT"=="RHFS"),]
w<- select(b, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
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

b<- q[(q$"DT"=="PRS" | q$"DT"=="PHFS"),]
w<- select(b, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
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

b<- q[(q$"DT"=="PAS" | q$"DT"=="PHFAS"),]
w<- select(b, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
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
i4$newcolumn<-"N vs HF"
colnames(i4)[colnames(i4)=="newcolumn"] <- "Type"

#HS 

b<- q[(q$"DT"=="RNS" | q$"DT"=="RHSNS"),]
w<- select(b, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
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

b<- q[(q$"DT"=="PRNS" | q$"DT"=="PHS13NS"),]
w<- select(b, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
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

b<- q[(q$"DT"=="PANS" | q$"DT"=="PHSA6NS"),]
w<- select(b, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
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

b<- q[(q$"DT"=="PANS" | q$"DT"=="PHSA13NS"),]
w<- select(b, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
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



#S
b<- q[(q$"DT"=="RS" | q$"DT"=="RHSS"),]
w<- select(b, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
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

b<- q[(q$"DT"=="PRS" | q$"DT"=="PHS13S"),]
w<- select(b, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
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

b<- q[(q$"DT"=="PAS" | q$"DT"=="PHSA6S"),]
w<- select(b, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
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

b<- q[(q$"DT"=="PAS" | q$"DT"=="PHSA13S"),]
w<- select(b, -c(id, e, "Genotype", Treatment, Round, Diet, DT, type))
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



i5<-rbind(u1,u2,u3,u4,u7,u8,u9,u10)

i5$newcolumn<-"N vs HS"
colnames(i5)[colnames(i5)=="newcolumn"] <- "Type"

o1<-rbind(i4,i5)

o1$comparison <- gsub("13", "11", o1$comparison)

write.csv(o1, "Anosim_all_d_zn_bray_d_g.csv")