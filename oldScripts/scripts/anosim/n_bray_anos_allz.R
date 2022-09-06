n_d_bray_z <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/n_d_bray_z.xlsx")

q<-n_d_bray_z
DT<-as.data.frame(paste(q$Diet, q$Treatment, sep=""))
q1<-cbind(DT, q)
colnames(q1)[1]<-"DT"

b<- q1[(q1$"DT"=="RNS" | q1$"DT"=="RS"),]
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

b<- q1[(q1$"DT"=="PRNS" | q1$"DT"=="PRS"),]
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


b<- q1[(q1$"DT"=="PANS" | q1$"DT"=="PAS"),]
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

i4<-rbind(i1,i2,i3)

write.csv(i4, "n_bray_anos_allz.csv")