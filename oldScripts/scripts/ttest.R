#ttesst
x2<-x1[(x1$DT=="PRNS"|x1$DT=="PHFNS"),]
w1<-t.test(Shannon~DT, data=x2, var.equal=FALSE)
w2<-as.data.frame(w1[["p.value"]])
e1<-as.data.frame(unique(x2$DT))
e2<-as.data.frame(t(e1))
w2$Comparison<-paste(e2$V1, e2$V2, sep=" vs ")
colnames(w2)[1] <- "p"
