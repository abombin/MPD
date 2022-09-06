library(stringr)

# total
q<- R_and_HF_HS_collection_total
colnames(q)[colnames(q)=="Sum(Total Collected)"] <- "total"
q1<- q[!is.na(q$"total"), ]
colnames(q1)[colnames(q1)=="Treatment"] <- "t"
colnames(q1)[colnames(q1)=="Genetic line"] <- "g"
colnames(q1)[colnames(q1)=="Diet"] <- "d"
colnames(q1)[colnames(q1)=="Round"] <- "r"
q1$Experiment <- gsub("1", "e1", q1$Experiment)
q1$Experiment <- gsub("2", "e2", q1$Experiment)
colnames(q1)[colnames(q1)=="Experiment"] <- "e"
q1$newcolumn<-paste(q1$d,q1$t,q1$g,q1$r,q1$e)
colnames(q1)[colnames(q1)=="newcolumn"] <- "dtgre"
q2<-aggregate( total ~ dtgre, q1, mean )

q3<-str_split_fixed(q2$dtgre, " ", 5)
q4<-cbind(q2,q3)
colnames(q4)[colnames(q4)=="1"] <- "d"
colnames(q4)[colnames(q4)=="2"] <- "t"
colnames(q4)[colnames(q4)=="3"] <- "g"
colnames(q4)[colnames(q4)=="4"] <- "r"
colnames(q4)[colnames(q4)=="5"] <- "e"
t1<- q4

# development
q<- R_and_HF_HS_median_days_to_develop_total_as_freq
colnames(q)[colnames(q)=="Median(Days to develop)"] <- "dev"
q1<- q[!is.na(q$"dev"), ]
colnames(q1)[colnames(q1)=="Treatment"] <- "t"
colnames(q1)[colnames(q1)=="Genetic Line"] <- "g"
colnames(q1)[colnames(q1)=="Diet"] <- "d"
colnames(q1)[colnames(q1)=="Round"] <- "r"
q1$Experiment <- gsub("1", "e1", q1$Experiment)
q1$Experiment <- gsub("2", "e2", q1$Experiment)
colnames(q1)[colnames(q1)=="Experiment"] <- "e"
q1$newcolumn<-paste(q1$d,q1$t,q1$g,q1$r,q1$e)
colnames(q1)[colnames(q1)=="newcolumn"] <- "dtgre"
q2<-aggregate( dev ~ dtgre, q1, mean )

q3<-str_split_fixed(q2$dtgre, " ", 5)
q4<-cbind(q2,q3)
colnames(q4)[colnames(q4)=="1"] <- "d"
colnames(q4)[colnames(q4)=="2"] <- "t"
colnames(q4)[colnames(q4)=="3"] <- "g"
colnames(q4)[colnames(q4)=="4"] <- "r"
colnames(q4)[colnames(q4)=="5"] <- "e"
t2<-q4

# weight

# imported file as q
q1<- q[!is.na(q$"weight"), ]
colnames(q1)[colnames(q1)=="Treatment"] <- "t"
colnames(q1)[colnames(q1)=="Genetic line"] <- "g"
colnames(q1)[colnames(q1)=="Diet"] <- "d"
colnames(q1)[colnames(q1)=="Round"] <- "r"
q1$Experiment <- gsub("1", "e1", q1$Experiment)
q1$Experiment <- gsub("2", "e2", q1$Experiment)
colnames(q1)[colnames(q1)=="Experiment"] <- "e"
q1$newcolumn<-paste(q1$d,q1$t,q1$g,q1$r,q1$e)
colnames(q1)[colnames(q1)=="newcolumn"] <- "dtgre"
q2<-aggregate( weight ~ dtgre, q1, mean )

q3<-str_split_fixed(q2$dtgre, " ", 5)
q4<-cbind(q2,q3)
colnames(q4)[colnames(q4)=="1"] <- "d"
colnames(q4)[colnames(q4)=="2"] <- "t"
colnames(q4)[colnames(q4)=="3"] <- "g"
colnames(q4)[colnames(q4)=="4"] <- "r"
colnames(q4)[colnames(q4)=="5"] <- "e"
t3<-q4

# trig
q<- trig
colnames(q)[colnames(q)=="Trig/larvae by weight"] <- "trig"
q1<- q[!is.na(q$"trig"), ]
colnames(q1)[colnames(q1)=="Treatment"] <- "t"
colnames(q1)[colnames(q1)=="Genetic line"] <- "g"
colnames(q1)[colnames(q1)=="Diet"] <- "d"
colnames(q1)[colnames(q1)=="Round"] <- "r"
q1$Experiment <- gsub("1", "e1", q1$Experiment)
q1$Experiment <- gsub("2", "e2", q1$Experiment)
colnames(q1)[colnames(q1)=="Experiment"] <- "e"
q1$newcolumn<-paste(q1$d,q1$t,q1$g,q1$r,q1$e)
colnames(q1)[colnames(q1)=="newcolumn"] <- "dtgre"
q2<-aggregate( trig ~ dtgre, q1, mean )

q3<-str_split_fixed(q2$dtgre, " ", 5)
q4<-cbind(q2,q3)
colnames(q4)[colnames(q4)=="1"] <- "d"
colnames(q4)[colnames(q4)=="2"] <- "t"
colnames(q4)[colnames(q4)=="3"] <- "g"
colnames(q4)[colnames(q4)=="4"] <- "r"
colnames(q4)[colnames(q4)=="5"] <- "e"
t4<-q4

#gluc
q<-gluc
colnames(q)[colnames(q)=="Glucose/larvae by weight"] <- "gluc"
q1<- q[!is.na(q$"gluc"), ]
colnames(q1)[colnames(q1)=="Treatment"] <- "t"
colnames(q1)[colnames(q1)=="Genetic line"] <- "g"
colnames(q1)[colnames(q1)=="Diet"] <- "d"
colnames(q1)[colnames(q1)=="Round"] <- "r"
q1$Experiment <- gsub("1", "e1", q1$Experiment)
q1$Experiment <- gsub("2", "e2", q1$Experiment)
colnames(q1)[colnames(q1)=="Experiment"] <- "e"
q1$newcolumn<-paste(q1$d,q1$t,q1$g,q1$r,q1$e)
colnames(q1)[colnames(q1)=="newcolumn"] <- "dtgre"
q2<-aggregate( gluc ~ dtgre, q1, mean )

q3<-str_split_fixed(q2$dtgre, " ", 5)
q4<-cbind(q2,q3)
colnames(q4)[colnames(q4)=="1"] <- "d"
colnames(q4)[colnames(q4)=="2"] <- "t"
colnames(q4)[colnames(q4)=="3"] <- "g"
colnames(q4)[colnames(q4)=="4"] <- "r"
colnames(q4)[colnames(q4)=="5"] <- "e"
t5<-q4

#prot
q<-prot
colnames(q)[colnames(q)=="Protein/larvae/weight"] <- "prot"
q1<- q[!is.na(q$"prot"), ]
colnames(q1)[colnames(q1)=="Treatment"] <- "t"
colnames(q1)[colnames(q1)=="Genetic line"] <- "g"
colnames(q1)[colnames(q1)=="Diet"] <- "d"
colnames(q1)[colnames(q1)=="Round"] <- "r"
q1$Experiment <- gsub("1", "e1", q1$Experiment)
q1$Experiment <- gsub("2", "e2", q1$Experiment)
colnames(q1)[colnames(q1)=="Experiment"] <- "e"
q1$newcolumn<-paste(q1$d,q1$t,q1$g,q1$r,q1$e)
colnames(q1)[colnames(q1)=="newcolumn"] <- "dtgre"
q2<-aggregate( prot ~ dtgre, q1, mean )

q3<-str_split_fixed(q2$dtgre, " ", 5)
q4<-cbind(q2,q3)
colnames(q4)[colnames(q4)=="1"] <- "d"
colnames(q4)[colnames(q4)=="2"] <- "t"
colnames(q4)[colnames(q4)=="3"] <- "g"
colnames(q4)[colnames(q4)=="4"] <- "r"
colnames(q4)[colnames(q4)=="5"] <- "e"
t6<-q4