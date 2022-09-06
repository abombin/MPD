library(dplyr)
library(readr)
library(readxl)

q<-tr_z_n_phen_rand_p
q1<- q
colnames(q1)[colnames(q1)=="Treatment"] <- "t"
colnames(q1)[colnames(q1)=="Genetic line"] <- "g"
colnames(q1)[colnames(q1)=="Diet"] <- "d"
colnames(q1)[colnames(q1)=="Round"] <- "r"

w5 <- q1 %>% group_by(taxa) %>% do(model = aov(trig~d*abundance, data = .))

r3<-sapply(w5$model, function(x) summary(x)[[1]][["Pr(>F)"]])

lapply(r3, write, "t.txt", append=TRUE, ncolumns=1000) 

t <- read_table2("t.txt", col_names = FALSE)
file.remove("t.txt")

w6<-as.data.frame(w5$taxa)
w7<- cbind(w6, t)