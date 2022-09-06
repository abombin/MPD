library(broom)
library(dplyr)
library(plyr)
library(rstatix)

q<-as.data.frame(hfhs_trans_zotu_normalized_sum_genera)
w1<-q[(q$DT=="PHSA6S"),]
w<- w1[, -c(1:10) ]

a<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
b<-w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]

A<-a
B<-b

results <- NULL



for (i in 1:ncol(A)){
  for (j in 1:ncol(B)){
    model_<-lm(A[,i]~B[,j])
    
    results<-bind_rows(results,
                       bind_cols(columnx = i, 
                                 columny = j,
                                 glance(model_),
                                 intercept=model_$coefficients[1],
                                 slope=model_$coefficients[2]
                       )
    )
  }
}



e1<-as.data.frame(colnames(A))
e1$id <- seq.int(nrow(e1))
e1$id1<- seq.int(nrow(e1))
e2<-results

colnames(e2)[1] <- "id"
colnames(e2)[2]<-"id1"
e3<-join(e2, e1, by = "id", type = "left", match = "all")
e4<-join(e3, e1, by = "id1", type = "left", match = "all")
e5<- e4[, -c(1:6, 8:16, 18,20) ]
colnames(e5)[2] <- "A"
colnames(e5)[3]<-"B"
e6<-e5[!(e5$A==e5$B),]
e7<-e6 %>% group_by(A) %>% adjust_pvalue( p.col= "p.value", output.col= "fdr p", method= "fdr")
e8<-data.frame(e7$A)
e8$B<-e7$B
e8$p.value<-e7$p.value
e8$fdr.p<-e7$`fdr p`

colnames(e8)[1] <- "Taxa1"
colnames(e8)[2] <- "Taxa2"
e9<- e8[!duplicated(apply(e8,1,function(x) paste(sort(x),collapse=''))),]

write.csv(e8, "phsa6_mic_o.csv")

x1.01<- e9[e9[,3] < 0.05, ]
x1.01a<-x1.01[!is.na(x1.01[,3]), ]
x1.01b<- x1.01a[!duplicated(apply(x1.01a,1,function(x) paste(sort(x),collapse=''))),]
x1.02<- x1.01b[x1.01b[,4] < 0.1, ]
i<-nrow(x1.01b)
i1<-as.data.frame(i)
colnames(i1)[colnames(i1)=="i"] <- "# sign p"
i1$newcolumn<- nrow(x1.02)
colnames(i1)[colnames(i1)=="newcolumn"] <- "# sign fdrp"









q1<-q1a[!(q1a$"type"=="food"),]
q1$Diet <- gsub("13", "11", q1$Diet)
q2<- as.data.frame(paste(q1$Diet, q1$Treatment, sep=""))
q<-cbind(q2,q1)
colnames(q)[1] <- "DT"
