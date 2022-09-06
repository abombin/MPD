### q3 is a data table wiht all taxonomic groups and categories, q5 has only 5 genetic lines common for both experiments
library(tidyr)
library(dplyr)
library(data.table)



q5<-all_tax_zn_5gen

w1 <- q3[ -c(3:9, 18) ]

w2 <- w1 %>% spread(key = zotu, value = abundance, fill=0)

w3 <- q5[ -c(3:9, 18) ]

w4 <- w3 %>% spread(key = zotu, value = abundance, fill=0)


e1<-aggregate(q5$abundance, by=list(y=q5$Family), FUN=sum)
is.na(e1) <- e1==""
e2<-e1[!is.na(e1$y), ]
e3<- e2[order(e2$"x", decreasing = T),]
e4<- e3[!(e3$y==" s"),]
e5<-head(e4, 100)
r1<-setDT(q5)[Family %chin% e5$y]

y2<-q5 %>% group_by(id, Diet, Genotype, Treatment, Round, e, type, Family) %>%  summarise(Total = sum(abundance, na.rm = TRUE))
y3<-unique(y2$Family)





