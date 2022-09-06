library(gtools)
library(data.table)
### genus
sp_z_n_rand_phen_d_t_g <- read_csv("C:/Users/abomb/Box/HF and HS/HF HS Paper/data tables/sp_z_n_rand_phen_d_t_g.csv")

hfhs_zn10_g <- read_delim("C:/Users/abomb/Box/HF and HS/R/HF HS/input/hfhs_zn10_g.txt", 
                          "\t", escape_double = FALSE, trim_ws = TRUE)

q1<-sp_z_n_rand_phen_d_t_g
w1<-hfhs_zn10_g

w2<-as.data.table(colnames(w1[, (7:16) ]))
q2<- setDT(q1)[Taxa %chin% w2$V1]
w3<- q1[(q1$"Taxa"=="Lachnospiraceae.NK4A136.group"| q1$"Taxa"=="Corynebacterium.1"),] # exception since taxa labeled wiredly if has spaces

q3<-rbind(q2,w3)
q3$p_star<-stars.pval(q3$p)
q3$fdrp_star<-stars.pval(q3$"fdr p")
write.csv(q3, "sp_z_n_rand_phen_d_t_g10_star.csv")
### NS 
