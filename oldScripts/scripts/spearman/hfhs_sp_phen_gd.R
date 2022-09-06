library(dplyr)
library(gtools)

tr_z_n_phen_rand_g <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/tr_z_n_phen_rand_g.xlsx", 
                                 col_types = c("text", "text", "text", 
                                               "text", "text", "text", "text", "text","text", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "text", "numeric"))

q1<-tr_z_n_phen_rand_g
q2<-q1[(q1$"taxa"=="Acetobacter"| q1$"taxa"=="Lactobacillus"),]
colnames(q2)[3] <- "Genotype"
q3<-q2 %>% group_by(taxa, Diet, Genotype) %>% filter(n() >= 4)
# gluc
a3<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, gluc, method="spearman")$p.val)
a4<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, gluc, method="spearman")$estimate)
colnames(a4)[4]<-"cor"
colnames(a3)[4]<-"p"
a3$cor<-a4$cor
a3$p_star<-stars.pval(a3$p)
a3$phen<-"gluc"
gluc<-a3

# trig
a3<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, trig, method="spearman")$p.val)
a4<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, trig, method="spearman")$estimate)
colnames(a4)[4]<-"cor"
colnames(a3)[4]<-"p"
a3$cor<-a4$cor
a3$p_star<-stars.pval(a3$p)
a3$phen<-"trig"
trig<-a3

# weight
a3<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, weight, method="spearman")$p.val)
a4<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, weight, method="spearman")$estimate)
colnames(a4)[4]<-"cor"
colnames(a3)[4]<-"p"
a3$cor<-a4$cor
a3$p_star<-stars.pval(a3$p)
a3$phen<-"weight"
weight<-a3

# protein
a3<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, prot, method="spearman")$p.val)
a4<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, prot, method="spearman")$estimate)
colnames(a4)[4]<-"cor"
colnames(a3)[4]<-"p"
a3$cor<-a4$cor
a3$p_star<-stars.pval(a3$p)
a3$phen<-"prot"
prot<-a3

# total
a3<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, total, method="spearman")$p.val)
a4<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, total, method="spearman")$estimate)
colnames(a4)[4]<-"cor"
colnames(a3)[4]<-"p"
a3$cor<-a4$cor
a3$p_star<-stars.pval(a3$p)
a3$phen<-"total"
total<-a3

# dev
a3<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, dev, method="spearman")$p.val)
a4<-q3%>%group_by(taxa, Diet, Genotype) %>%summarize( cor.test(abundance, dev, method="spearman")$estimate)
colnames(a4)[4]<-"cor"
colnames(a3)[4]<-"p"
a3$cor<-a4$cor
a3$p_star<-stars.pval(a3$p)
a3$phen<-"dev"
dev<-a3

q4<-rbind(trig,gluc,prot,weight,total,dev)

write.csv(q4, "hfhs_sp_phen_gd_g.csv")
