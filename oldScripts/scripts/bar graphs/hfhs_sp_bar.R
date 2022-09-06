library(ggplot2)

group.colors <- c("_" = "#000000", "." = "#999999", "*" ="#0072B2", "**" = "#009E73", "***" = "#D55E00") # make color panel

q3<-sp_z_n_rand_phen_d_t_g10_star
q3$taxa<-substr(q3$Taxa, 1, 4) # modify taxa name
q3$p_star[is.na(q3$p_star)] = "_" # replace na with symbols
q3$p_star <- gsub(" ", "_", q3$p_star) # replace " " with symbols
q3$Diet <- gsub("PHS11", "PHS", q3$Diet)
### 
w4<- q3[(q3$"Phenotype"=="Total"& q3$"Type"=="HF"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHF", "PHF", "PHFA")) # organize figure legend

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("hf_sp_total.png", dpi = 300, width=50, height=25, units= c("cm"))

### 
w4<- q3[(q3$"Phenotype"=="Development"& q3$"Type"=="HF"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHF", "PHF", "PHFA")) # organize figure legend

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("hf_sp_dev.png", dpi = 300, width=50, height=25, units= c("cm"))
###
w4<- q3[(q3$"Phenotype"=="Weight"& q3$"Type"=="HF"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHF", "PHF", "PHFA")) # organize figure legend

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("hf_sp_weight.png", dpi = 300, width=50, height=25, units= c("cm"))
###
w4<- q3[(q3$"Phenotype"=="Triglyceride"& q3$"Type"=="HF"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHF", "PHF", "PHFA")) # organize figure legend

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("hf_sp_trig.png", dpi = 300, width=50, height=25, units= c("cm"))
###
w4<- q3[(q3$"Phenotype"=="Protein"& q3$"Type"=="HF"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHF", "PHF", "PHFA")) # organize figure legend

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("hf_sp_prot.png", dpi = 300, width=50, height=25, units= c("cm"))
###
w4<- q3[(q3$"Phenotype"=="Glucose"& q3$"Type"=="HF"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHF", "PHF", "PHFA")) # organize figure legend

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("hf_sp_gluc.png", dpi = 300, width=50, height=25, units= c("cm"))


### HS
### 
w4<- q3[(q3$"Phenotype"=="Total"& q3$"Type"=="HS"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHS", "PHS", "PHSA6", "PHSA11")) # organize figure legend

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("hs_sp_total.png", dpi = 300, width=50, height=25, units= c("cm"))

### 
w4<- q3[(q3$"Phenotype"=="Development"& q3$"Type"=="HS"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHS", "PHS", "PHSA6", "PHSA11")) # organize figure legend

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("hs_sp_dev.png", dpi = 300, width=50, height=25, units= c("cm"))
###
w4<- q3[(q3$"Phenotype"=="Weight"& q3$"Type"=="HS"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHS", "PHS", "PHSA6", "PHSA11")) # organize figure legend

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("hs_sp_weight.png", dpi = 300, width=50, height=25, units= c("cm"))
###
w4<- q3[(q3$"Phenotype"=="Triglyceride"& q3$"Type"=="HS"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHS", "PHS", "PHSA6", "PHSA11")) # organize figure legend

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("hs_sp_trig.png", dpi = 300, width=50, height=25, units= c("cm"))
###
w4<- q3[(q3$"Phenotype"=="Protein"& q3$"Type"=="HS"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHS", "PHS", "PHSA6", "PHSA11")) # organize figure legend

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("hs_sp_prot.png", dpi = 300, width=50, height=25, units= c("cm"))
###
w4<- q3[(q3$"Phenotype"=="Glucose"& q3$"Type"=="HS"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHS", "PHS", "PHSA6", "PHSA11")) # organize figure legend

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")
p+facet_grid(col=vars(taxa), rows=vars(Treatment))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("hs_sp_gluc.png", dpi = 300, width=50, height=25, units= c("cm"))

### AxGxD
hfhs_sp_phen_gd_g
q3<-q4
q3$p_star[is.na(q3$p_star)] = "_" # replace na with symbols
q3$p_star <- gsub(" ", "_", q3$p_star) # replace " " with symbols
q3$Diet <- gsub("13", "11", q3$Diet)
q3$Diet <- gsub("PHS11", "PHS", q3$Diet)

# gluc
w4<- q3[(q3$"phen"=="gluc"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHF", "PHF", "PHFA","RHS", "PHS", "PHSA6", "PHSA11")) # organize figure legend

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")
p+facet_grid(col=vars(Genotype), rows=vars(taxa))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("dga_sp_gluc.png", dpi = 300, width=50, height=25, units= c("cm"))

# trig
w4<- q3[(q3$"phen"=="trig"),] # select group
w4$p_star <- factor(w4$p_star, levels = c("***", "**", "*",".","_")) # organize figure legend
w4$Diet <- factor(w4$Diet, levels = c("RHF", "PHF", "PHFA","RHS", "PHS", "PHSA6", "PHSA11")) # organize figure legend

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 24))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(colour="black", size=28, face="bold"))+
  theme(legend.title = element_text(colour="black", size=28, face="bold"))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  labs(fill = "Signif")
p+facet_grid(col=vars(Genotype), rows=vars(taxa))+
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))

ggsave("dga_sp_trig.png", dpi = 300, width=50, height=25, units= c("cm"))