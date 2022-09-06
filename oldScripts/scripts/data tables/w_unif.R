library(readr)
library(readxl)
library(plyr)

q1<-all_d_wunif

hfhs_id<-HF_HS_samples_key_updated_for_sequences_results
hfhs_id$e<-"e2"
colnames(hfhs_id)[1] <-"id"
colnames(hfhs_id)[3] <-"Genotype"
colnames(hfhs_id)[6] <-"type"

z1<-as.data.frame(q1$X1)
q1<-cbind(z1,q1)
colnames(q1)[1] <-"id"
n_id$e<-"e1"
n_id$type<-"N"
q1$id <- gsub('_tr', '', q1$id)

id_all<- rbind.fill(hfhs_id, n_id )


q3<-merge(q1, id_all[, c("id", "Diet", "Genotype", "Treatment", "Round", "e", "type", "Group", "Sample")], by="id", all.x = TRUE) 

m1<-unique(q3[c("id", "Diet")])

write.table(q3, "all_d_wunif_w_cat.txt", append = FALSE, sep = "\t", dec = ".",
            row.names = FALSE, col.names = TRUE)

a1<-all_tax_zn
q5<-q3[(q3$"Genotype"=="153"|q3$"Genotype"=="748"|q3$"Genotype"=="787"|q3$"Genotype"=="802"|q3$"Genotype"=="805"),]
write.table(q5, "all_d_wunif_5gen.txt", append = FALSE, sep = "\t", dec = ".",
            row.names = FALSE, col.names = TRUE)