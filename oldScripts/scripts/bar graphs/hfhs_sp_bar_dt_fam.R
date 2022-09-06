library(ggplot2)
library(readr)
library(readxl)
library(readr)
library(gtools)
library(data.table)

### Family
q1 <- read_csv("C:/Users/abomb/Box/HF and HS/HF HS Paper/data tables/sp_z_n_rand_phen_d_t_f.csv")

w1 <- read_delim("C:/Users/abomb/Box/HF and HS/R/HF HS/input/hfhs_zn10_f.txt", 
                          "\t", escape_double = FALSE, trim_ws = TRUE)

w2<-as.data.table(colnames(w1[, (7:16) ]))
q2<- setDT(q1)[Taxa %chin% w2$V1]
q2$p_star<-stars.pval(q2$p)
q3<-q2

group.colors <- c("_" = "#000000", "." = "#999999", "*" ="#0072B2", "**" = "#009E73", "***" = "#D55E00") # make color panel
q3$taxa<-substr(q3$Taxa, 1, 4) # modify taxa name
q3$p_star[is.na(q3$p_star)] = "_" # replace na with symbols
q3$p_star <- gsub(" ", "_", q3$p_star) # replace " " with symbols
q3$Diet <- gsub("PHS11", "PHS", q3$Diet)


## Glucose
w4<- q3[(q3$"Phenotype"=="Glucose"& q3$"Type"!="N"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHF", "RHS", "PHF", "PHS", "PHFA", "PHSA6","PHSA11")) # organize figure legend
w5<-w4[(w4$taxa=="Cory"| w4$taxa=="Lact" | w4$taxa=="Lach"| w4$taxa=="Erys"),]

p<-ggplot(data=w5, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 36))+
  theme(axis.text.x = element_text(size = 36))+
  theme(axis.text.x = element_text(angle = 90, vjust=0.4))+
  theme(legend.text = element_text(colour="black", size=36, face="bold"))+
  theme(legend.title = element_text(colour="black", size=40, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")+
  ylab("Glucose")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 44))+
  theme(strip.text.y = element_text(size = 44))

ggsave("hfhs_sp_Glucose_ftax.png", dpi = 300, width=50, height=25, units= c("cm"))

## Trig
w4<- q3[(q3$"Phenotype"=="Triglyceride"& q3$"Type"!="N"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHF", "RHS", "PHF", "PHS", "PHFA", "PHSA6","PHSA11")) # organize figure legend
w5<-w4[(w4$taxa=="Lact" | w4$taxa=="Lach"| w4$taxa=="Ente"| w4$taxa=="Erys"),]

p<-ggplot(data=w5, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 36))+
  theme(axis.text.x = element_text(size = 36))+
  theme(axis.text.x = element_text(angle = 90, vjust=0.4))+
  theme(legend.text = element_text(colour="black", size=36, face="bold"))+
  theme(legend.title = element_text(colour="black", size=40, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")+
  ylab("Triglyceride")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 44))+
  theme(strip.text.y = element_text(size = 44))

ggsave("hfhs_sp_trig_ftax.png", dpi = 300, width=50, height=25, units= c("cm"))


## Weight
w4<- q3[(q3$"Phenotype"=="Weight"& q3$"Type"!="N"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHF", "RHS", "PHF", "PHS", "PHFA", "PHSA6","PHSA11")) # organize figure legend
w5<-w4[(w4$taxa=="Lact" | w4$taxa=="Lach"| w4$taxa=="Ente"| w4$taxa=="Erys"),]

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 36))+
  theme(axis.text.x = element_text(size = 36))+
  theme(axis.text.x = element_text(angle = 90, vjust=0.4))+
  theme(legend.text = element_text(colour="black", size=36, face="bold"))+
  theme(legend.title = element_text(colour="black", size=40, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")+
  ylab("Weight")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 44))+
  theme(strip.text.y = element_text(size = 44))

ggsave("hfhs_sp_weight_ftax.png", dpi = 300, width=50, height=25, units= c("cm"))

## Total
w4<- q3[(q3$"Phenotype"=="Total"& q3$"Type"!="N"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHF", "RHS", "PHF", "PHS", "PHFA", "PHSA6","PHSA11")) # organize figure legend
w5<-w4[(w4$taxa=="Lact" | w4$taxa=="Lach"| w4$taxa=="Ente"| w4$taxa=="Erys"),]

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 36))+
  theme(axis.text.x = element_text(size = 36))+
  theme(axis.text.x = element_text(angle = 90, vjust=0.4))+
  theme(legend.text = element_text(colour="black", size=36, face="bold"))+
  theme(legend.title = element_text(colour="black", size=40, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")+
  ylab("Total")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 44))+
  theme(strip.text.y = element_text(size = 44))

ggsave("hfhs_sp_total_ftax.png", dpi = 300, width=50, height=25, units= c("cm"))


## Prot
w4<- q3[(q3$"Phenotype"=="Protein"& q3$"Type"!="N"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHF", "RHS", "PHF", "PHS", "PHFA", "PHSA6","PHSA11")) # organize figure legend
w5<-w4[(w4$taxa=="Lact" | w4$taxa=="Lach"| w4$taxa=="Ente"| w4$taxa=="Erys"),]

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 36))+
  theme(axis.text.x = element_text(size = 36))+
  theme(axis.text.x = element_text(angle = 90, vjust=0.4))+
  theme(legend.text = element_text(colour="black", size=36, face="bold"))+
  theme(legend.title = element_text(colour="black", size=40, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")+
  ylab("Protein")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 44))+
  theme(strip.text.y = element_text(size = 44))

ggsave("hfhs_sp_prot_ftax.png", dpi = 300, width=50, height=25, units= c("cm"))