library(dplyr)
library(vegan)
library(DescTools)
library(data.table)
# Shannon
q1<-transpose_all_zotu_norm
q<- q1[(q1$Treatment!="food"),]
qa<- select(q, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", DT, Type, Sample, Diet))
r<-diversity(qa, index = "shannon", MARGIN = 1, base = exp(1))
x<-as.matrix(r)
x1<-as.data.frame(x)
x1$DT = q$DT
x1$Diet=q$Diet
x1$Treatment=q$Treatment
x1$Type=q$Type
t1<- x1%>% group_by(DT)%>% summarise(Shannon = mean(V1, na.rm = TRUE))
colnames(x1)<-c("Shannon", "DT", "Diet", "Treatment", "Type")


w1<- x1[(x1$Treatment=="NS" & x1$Type=="HF"),]
w2<- aov(Shannon~Diet, data=w1)
w3<- PostHocTest(w2, method="lsd")
w4<-as.data.frame(w3$Diet)
w5<- setDT(w4, keep.rownames = TRUE)[]
w5$Treatment<-unique(w1$Treatment)
e1<-w5


w1<- x1[(x1$Treatment=="S" & x1$Type=="HF"),]
w2<- aov(Shannon~Diet, data=w1)
w3<- PostHocTest(w2, method="lsd")
w4<-as.data.frame(w3$Diet)
w5<- setDT(w4, keep.rownames = TRUE)[]
w5$Treatment<-unique(w1$Treatment)
e2<-w5

w1<- x1[(x1$Treatment=="NS" & x1$Type=="HS"),]
w2<- aov(Shannon~Diet, data=w1)
w3<- PostHocTest(w2, method="lsd")
w4<-as.data.frame(w3$Diet)
w5<- setDT(w4, keep.rownames = TRUE)[]
w5$Treatment<-unique(w1$Treatment)
e3<-w5

w1<- x1[(x1$Treatment=="S" & x1$Type=="HS"),]
w2<- aov(Shannon~Diet, data=w1)
w3<- PostHocTest(w2, method="lsd")
w4<-as.data.frame(w3$Diet)
w5<- setDT(w4, keep.rownames = TRUE)[]
w5$Treatment<-unique(w1$Treatment)
e4<-w5

r1<- rbind(e1,e2,e3,e4)
colnames(r1)[1] <- "Comparison"
colnames(r1)[5] <- "Shannon p"
r2<- r1[, -c(2:4) ]
y1<-r2

#Simpson
r<-diversity(qa, index = "simpson", MARGIN = 1)
x<-as.matrix(r)
x1<-as.data.frame(x)
x1$DT = q$DT
x1$Diet=q$Diet
x1$Treatment=q$Treatment
x1$Type=q$Type
t2<- x1%>% group_by(DT)%>% summarise(Simpson = mean(V1, na.rm = TRUE))
colnames(x1)<-c("Simpson", "DT", "Diet", "Treatment", "Type")


w1<- x1[(x1$Treatment=="NS" & x1$Type=="HF"),]
w2<- aov(Simpson~Diet, data=w1)
w3<- PostHocTest(w2, method="lsd")
w4<-as.data.frame(w3$Diet)
w5<- setDT(w4, keep.rownames = TRUE)[]
w5$Treatment<-unique(w1$Treatment)
e1<-w5

w1<- x1[(x1$Treatment=="S" & x1$Type=="HF"),]
w2<- aov(Simpson~Diet, data=w1)
w3<- PostHocTest(w2, method="lsd")
w4<-as.data.frame(w3$Diet)
w5<- setDT(w4, keep.rownames = TRUE)[]
w5$Treatment<-unique(w1$Treatment)
e2<-w5

w1<- x1[(x1$Treatment=="NS" & x1$Type=="HS"),]
w2<- aov(Simpson~Diet, data=w1)
w3<- PostHocTest(w2, method="lsd")
w4<-as.data.frame(w3$Diet)
w5<- setDT(w4, keep.rownames = TRUE)[]
w5$Treatment<-unique(w1$Treatment)
e3<-w5

w1<- x1[(x1$Treatment=="S" & x1$Type=="HS"),]
w2<- aov(Simpson~Diet, data=w1)
w3<- PostHocTest(w2, method="lsd")
w4<-as.data.frame(w3$Diet)
w5<- setDT(w4, keep.rownames = TRUE)[]
w5$Treatment<-unique(w1$Treatment)
e4<-w5

r1<- rbind(e1,e2,e3,e4)
colnames(r1)[1] <- "Comparison"
colnames(r1)[5] <- "Simpson p"
r2<- r1[, -c(2:4) ]
y2<-r2

# richness
r<-specnumber(qa, MARGIN = 1)
x<-as.matrix(r)
x1<-as.data.frame(x)
x1$DT = q$DT
x1$Diet=q$Diet
x1$Treatment=q$Treatment
x1$Type=q$Type
t3<- x1%>% group_by(DT)%>% summarise(Richness = mean(V1, na.rm = TRUE))
colnames(x1)<-c("Richness", "DT", "Diet", "Treatment", "Type")


w1<- x1[(x1$Treatment=="NS" & x1$Type=="HF"),]
w2<- aov(Richness~Diet, data=w1)
w3<- PostHocTest(w2, method="lsd")
w4<-as.data.frame(w3$Diet)
w5<- setDT(w4, keep.rownames = TRUE)[]
w5$Treatment<-unique(w1$Treatment)
e1<-w5

w1<- x1[(x1$Treatment=="S" & x1$Type=="HF"),]
w2<- aov(Richness~Diet, data=w1)
w3<- PostHocTest(w2, method="lsd")
w4<-as.data.frame(w3$Diet)
w5<- setDT(w4, keep.rownames = TRUE)[]
w5$Treatment<-unique(w1$Treatment)
e2<-w5

w1<- x1[(x1$Treatment=="NS" & x1$Type=="HS"),]
w2<- aov(Richness~Diet, data=w1)
w3<- PostHocTest(w2, method="lsd")
w4<-as.data.frame(w3$Diet)
w5<- setDT(w4, keep.rownames = TRUE)[]
w5$Treatment<-unique(w1$Treatment)
e3<-w5

w1<- x1[(x1$Treatment=="S" & x1$Type=="HS"),]
w2<- aov(Richness~Diet, data=w1)
w3<- PostHocTest(w2, method="lsd")
w4<-as.data.frame(w3$Diet)
w5<- setDT(w4, keep.rownames = TRUE)[]
w5$Treatment<-unique(w1$Treatment)
e4<-w5

r1<- rbind(e1,e2,e3,e4)
colnames(r1)[1] <- "Comparison"
colnames(r1)[5] <- "Richness p"
r2<- r1[, -c(2:4) ]
y3<-r2

y4<-cbind(y1,y2,y3)
y5<-y4[, -c(3,4,6,7) ]
y5$Comparison <- gsub('-', ' vs ', y5$Comparison)
y5$Comparison <- gsub('13', '11', y5$Comparison)
write.csv(y5, "hfhs_all_zn_alpd_d.csv")

u1<-cbind(t1,t2,t3)
u2<-u1[, -c(3,5) ]
u2$DT <- gsub('13', '11', u2$DT)
write.csv(u2, "hfhs_all_zn_alpd_perd.csv")

