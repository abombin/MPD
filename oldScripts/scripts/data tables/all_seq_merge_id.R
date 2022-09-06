library(readr)
library(readxl)
library(plyr)

q1<-n_hfhs_z_n

hfhs_id<-HF_HS_samples_key_updated_for_sequences_results
hfhs_id$e<-"e2"
colnames(hfhs_id)[1] <-"id"
colnames(hfhs_id)[3] <-"Genotype"
colnames(hfhs_id)[6] <-"type"

colnames(q1)[9] <-"id"
n_id$e<-"e1"
n_id$type<-"N"
q1$id <- gsub('_tr', '', q1$id)

id_all<- rbind.fill(hfhs_id, n_id )


q3<-merge(q1, id_all[, c("id", "Diet", "Genotype", "Treatment", "Round", "e", "type", "Group", "Sample")], by="id", all.x = TRUE) 

m1<-unique(q3[c("id", "Diet")])

write.table(q3, "all_tax_zn.txt", append = FALSE, sep = "\t", dec = ".",
            row.names = FALSE, col.names = TRUE)

a1<-all_tax_zn
q5<-a1[(a1$"Genotype"=="153"|a1$"Genotype"=="748"|a1$"Genotype"=="787"|a1$"Genotype"=="802"|a1$"Genotype"=="805"),]
write.table(q5, "all_tax_zn_5gen.txt", append = FALSE, sep = "\t", dec = ".",
            row.names = FALSE, col.names = TRUE)


