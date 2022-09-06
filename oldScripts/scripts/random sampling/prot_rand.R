library(dplyr)
library(plyr)


q1<-R_and_HF_HS_prot_no_food
q<-q1
colnames(q)[colnames(q)=="Protein/larvae/weight"] <- "A"
q$newcolumn<- paste(q$Diet, q$Treatment, q$"Genetic line", sep="")
colnames(q)[colnames(q)=="newcolumn"] <- "DTG"
#HS NS
set.seed(7)
w<- q[(q$"DTG"=="RHSNS153" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t1<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHSNS748" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t2<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHSNS787" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t3<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHSNS802" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t4<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHSNS805" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t5<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHS13NS153" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t6<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHS13NS748" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t7<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHS13NS787" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t8<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHS13NS802" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t9<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHS13NS805" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t10<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA13NS153" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t11<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA13NS748" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t12<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA13NS787" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t13<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA13NS802" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t14<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA13NS805" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t15<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA6NS153" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t16<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA6NS748" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t17<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA6NS787" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t18<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA6NS802" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t19<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA6NS805" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t20<-r4

y1<- rbind.fill(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20)


#HS S

set.seed(7)
w<- q[(q$"DTG"=="RHSS153" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t1<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHSS748" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t2<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHSS787" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t3<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHSS802" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t4<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHSS805" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t5<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHS13S153" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t6<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHS13S748" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t7<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHS13S787" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t8<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHS13S802" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t9<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHS13S805" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t10<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA13S153" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t11<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA13S748" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t12<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA13S787" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t13<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA13S802" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t14<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA13S805" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t15<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA6S153" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t16<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA6S748" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t17<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA6S787" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t18<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA6S802" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t19<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHSA6S805" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t20<-r4

y2<- rbind.fill(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20)

u1<-rbind.fill(y1,y2)

u1$newcolumn<-"HS"
colnames(u1)[colnames(u1)=="newcolumn"] <- "Type"
u1$newcolumn<-"2"
colnames(u1)[colnames(u1)=="newcolumn"] <- "Experiment"

#HF NS

set.seed(7)
w<- q[(q$"DTG"=="RHFNS153" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t1<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHFNS748" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t2<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHFNS787" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t3<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHFNS802" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t4<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHFNS805" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t5<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFNS153" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t6<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFNS748" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t7<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFNS787" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t8<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFNS802" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t9<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFNS805" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t10<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFANS153" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t11<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFANS748" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t12<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFANS787" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t13<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFANS802" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t14<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFANS805" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t15<-r4

y1<- rbind.fill(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15)

#HF S

set.seed(7)
w<- q[(q$"DTG"=="RHFS153" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t1<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHFS748" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t2<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHFS787" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t3<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHFS802" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t4<-r4

set.seed(7)
w<- q[(q$"DTG"=="RHFS805" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t5<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFS153" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t6<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFS748" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t7<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFS787" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t8<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFS802" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t9<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFS805" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t10<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFAS153" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t11<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFAS748" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t12<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFAS787" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t13<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFAS802" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t14<-r4

set.seed(7)
w<- q[(q$"DTG"=="PHFAS805" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"

t15<-r4

y2<- rbind.fill(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15)

u2<-rbind.fill(y1,y2)

u2$newcolumn<-"HF"
colnames(u2)[colnames(u2)=="newcolumn"] <- "Type"
u2$newcolumn<-"1"
colnames(u2)[colnames(u2)=="newcolumn"] <- "Experiment"

# Controls
# HF Control NS
set.seed(7)
w<- q[(q$"DTG"=="RNS153" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t1<-r4

set.seed(7)
w<- q[(q$"DTG"=="RNS748" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t2<-r4

set.seed(7)
w<- q[(q$"DTG"=="RNS787" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t3<-r4

set.seed(7)
w<- q[(q$"DTG"=="RNS802" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t4<-r4

set.seed(7)
w<- q[(q$"DTG"=="RNS805" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t5<-r4


set.seed(7)
w<- q[(q$"DTG"=="PRNS153" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t6<-r4

set.seed(7)
w<- q[(q$"DTG"=="PRNS748" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t7<-r4

set.seed(7)
w<- q[(q$"DTG"=="PRNS787" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t8<-r4

set.seed(7)
w<- q[(q$"DTG"=="PRNS802" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t9<-r4

set.seed(7)
w<- q[(q$"DTG"=="PRNS805" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t10<-r4


set.seed(7)
w<- q[(q$"DTG"=="PANS153" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t11<-r4

set.seed(7)
w<- q[(q$"DTG"=="PANS748" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t12<-r4

set.seed(7)
w<- q[(q$"DTG"=="PANS787" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t13<-r4

set.seed(7)
w<- q[(q$"DTG"=="PANS802" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t14<-r4

set.seed(7)
w<- q[(q$"DTG"=="PANS805" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t15<-r4

y1<- rbind.fill(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15)
# control HF S

set.seed(7)
w<- q[(q$"DTG"=="RS153" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t1<-r4

set.seed(7)
w<- q[(q$"DTG"=="RS748" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t2<-r4

set.seed(7)
w<- q[(q$"DTG"=="RS787" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t3<-r4

set.seed(7)
w<- q[(q$"DTG"=="RS802" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t4<-r4

set.seed(7)
w<- q[(q$"DTG"=="RS805" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t5<-r4


set.seed(7)
w<- q[(q$"DTG"=="PRS153" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t6<-r4

set.seed(7)
w<- q[(q$"DTG"=="PRS748" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t7<-r4

set.seed(7)
w<- q[(q$"DTG"=="PRS787" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t8<-r4

set.seed(7)
w<- q[(q$"DTG"=="PRS802" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t9<-r4

set.seed(7)
w<- q[(q$"DTG"=="PRS805" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t10<-r4


set.seed(7)
w<- q[(q$"DTG"=="PAS153" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t11<-r4

set.seed(7)
w<- q[(q$"DTG"=="PAS748" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t12<-r4

set.seed(7)
w<- q[(q$"DTG"=="PAS787" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t13<-r4

set.seed(7)
w<- q[(q$"DTG"=="PAS802" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t14<-r4

set.seed(7)
w<- q[(q$"DTG"=="PAS805" & q$Experiment=="1" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t15<-r4

y2<- rbind.fill(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15)

u3<-rbind.fill(y1,y2)
u3$newcolumn<-"N1"
colnames(u3)[colnames(u3)=="newcolumn"] <- "Type"

# Conrol experiment 2
set.seed(7)
w<- q[(q$"DTG"=="RNS153" & q$Experiment=="2" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t1<-r4

set.seed(7)
w<- q[(q$"DTG"=="RNS748" & q$Experiment=="2" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t2<-r4

set.seed(7)
w<- q[(q$"DTG"=="RNS787" & q$Experiment=="2" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t3<-r4

set.seed(7)
w<- q[(q$"DTG"=="RNS802" & q$Experiment=="2" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t4<-r4

set.seed(7)
w<- q[(q$"DTG"=="RNS805" & q$Experiment=="2" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t5<-r4


set.seed(7)
w<- q[(q$"DTG"=="PRNS153" & q$Experiment=="2" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t6<-r4

set.seed(7)
w<- q[(q$"DTG"=="PRNS748" & q$Experiment=="2" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t7<-r4

set.seed(7)
w<- q[(q$"DTG"=="PRNS787" & q$Experiment=="2" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t8<-r4

set.seed(7)
w<- q[(q$"DTG"=="PRNS802" & q$Experiment=="2" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t9<-r4

set.seed(7)
w<- q[(q$"DTG"=="PRNS805" & q$Experiment=="2" ),]
if (nrow(w)>3) {e2<-split(w$A, sample(rep(1:3, c(2))))} else {e2<-split(w$A, sample(rep(1:3, c(1))))}
r1<- mean(e2$`1`)
r2<- mean(e2$`2`)
r3<- mean(e2$`3`)
r4<- as.data.frame(rbind(r1,r2,r3))
r4$newcolumn<- unique(w$Diet)
colnames(r4)[colnames(r4)=="newcolumn"] <- "Diet"
r4$newcolumn<- unique(w$"Genetic line")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Genotype"
r4$newcolumn<- unique(w$"Treatment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Treatment"
r4$newcolumn<- unique(w$"DTG")
colnames(r4)[colnames(r4)=="newcolumn"] <- "DTG"
r4$newcolumn<- unique(w$"Experiment")
colnames(r4)[colnames(r4)=="newcolumn"] <- "Experiment"
t10<-r4

u4<- rbind.fill(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)

u4$newcolumn<-"N2"
colnames(u4)[colnames(u4)=="newcolumn"] <- "Type"

i1<- rbind(u1,u2,u3,u4)

i1$Experiment <- gsub("1", "e1", i1$Experiment)
i1$Experiment <- gsub("2", "e2", i1$Experiment)
i1$newcolumn<- paste(i1$DTG,i1$Experiment)
colnames(i1)[colnames(i1)=="newcolumn"] <- "DTGE"

colnames(i1)[colnames(i1)=="V1"] <- "prot"


write.csv(i1, "n_hf_hs_prot_rand.csv")