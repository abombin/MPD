library(dplyr)

cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

group.colors <- c("_" = "#000000", "." = "#999999", "*" ="#0072B2", "**" = "#009E73", "***" = "#D55E00")

### NS 
w4<- q3[(q3$"Treatment"=="NS"& q3$"Phenotype"=="Total"& q3$"Type"=="HF"),]
w4$taxa<-substr(w4$Taxa, 1, 4)

ggplot(data=w4, aes(x=Taxa, y=cor)) +
  geom_bar(stat="identity")+
  facet_wrap(~Diet)

ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  facet_grid(. ~ Taxa)

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")
p+facet_grid(col=vars(Taxa))

w4<- q3[(q3$"Phenotype"=="Total"& q3$"Type"=="HF"),]
w4$taxa<-substr(w4$Taxa, 1, 4)

w4$p_star <- gsub(" ", "_", w4$p_star)

p<-ggplot(data=w4, aes(x=Diet, y=cor, fill=p_star)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = group.colors)+
  geom_hline(yintercept = 0, color = "black")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))
p+facet_grid(col=vars(taxa), rows=vars(Treatment))

ggsave("barexp.png", dpi = 300, width=50, height=25, units= c("cm"))


# per diet
w4<- q3[(q3$"Treatment"=="NS"& q3$"Phenotype"=="Total"& q3$"Diet"=="PHS11"),]

ggplot(data=w4, aes(x=Taxa, y=cor)) +
  geom_bar(aes(fill=p_star),stat="identity")

ggsave("barexp.png", dpi = 300, width=50, height=25, units= c("cm"))

