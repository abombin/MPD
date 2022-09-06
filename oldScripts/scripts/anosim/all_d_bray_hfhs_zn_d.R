library(dplyr)
library(vegan)

y<- all_d_zn_z
q<-y
q$DT<- paste(q$Diet, q$Treatment, sep="")

### HS NS
b<- q[(q$"DT"=="RHFNS" | q$"DT"=="RHSNS"),]
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

b<- q[(q$"DT"=="PHFNS" | q$"DT"=="PHS13NS"),]
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

b<- q[(q$"DT"=="PHFANS" | q$"DT"=="PHSA6NS"),]
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

b<- q[(q$"DT"=="PHFANS" | q$"DT"=="PHSA13NS"),]
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
b<- q[(q$"DT"=="RHFS" | q$"DT"=="RHSS"),]
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

b<- q[(q$"DT"=="PHFS" | q$"DT"=="PHS13S"),]
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

b<- q[(q$"DT"=="PHFAS" | q$"DT"=="PHSA6S"),]
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

b<- q[(q$"DT"=="PHFAS" | q$"DT"=="PHSA13S"),]
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

i5$comparison <- gsub("13", "11", i5$comparison)

write.csv(i5, "Anosim_all_d_zn_bray_d_hfhs.csv")