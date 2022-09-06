
library(dplyr)


q<-tr_z_n_phen_rand_p
q1<- q
colnames(q1)[colnames(q1)=="Treatment"] <- "t"
colnames(q1)[colnames(q1)=="Genetic line"] <- "g"
colnames(q1)[colnames(q1)=="Diet"] <- "d"
colnames(q1)[colnames(q1)=="Round"] <- "r"

w5<-lapply(split(q1, q1$taxa), aov, formula=trig ~ d*abundance)

r3<-sapply(w5, function(x) summary(x)[[1]][["Pr(>F)"]])

lapply(r3, write, "t.txt", append=TRUE, ncolumns=1000) 

t <- read_table2("t.txt", col_names = FALSE)

x6<-as.data.frame(unique(q1$taxa))

y1<-cbind(x6, t)


