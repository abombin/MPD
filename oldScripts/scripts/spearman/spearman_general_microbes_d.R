library(Hmisc)
library(rstatix)
library(dplyr)
library(gtools)
library(data.table)
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
} 

q<- hfhs_trans_zotu_normalized_sum_phylum
w<-q

w1<- w[(a$DT=="RHFNS"),]
a<- select(w1, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", Diet, DT, Type, Sample))
