library(broom)
library(dplyr)
library(plyr)
library(rstatix)

q<-as.data.frame(transpose_all_zotu_norm)
w1<-q[(q$DT=="PHSA6S"),]
w<- w1[, -c(1:9) ]
w<-select(w,-DT)

a<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
b<-w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]

c<-cor.test(a, b, method="pearson")
d<-cor(a, method="pearson")

e<-cor_pmat(a, method="pearson")
pb <- progress_bar$new(total = 100)

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
  pb$tick()}