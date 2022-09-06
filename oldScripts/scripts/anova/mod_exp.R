
w5 <- q2 %>% group_by(taxa) %>% do(model = summary(aov(trig~d*abundance, data = .),))
t1<- sapply(w5$model, function(x) summary(x))
t1a<-w5$model
t1b<- lapply(t1a, function(x) as.data.frame(unlist(x)))

t2a<- sapply(t1b, function(x) x[(x=="Pr(>F)"),])


t2<- sapply(t1a, function(x) x$"Pr(>F)")
t3<-as.data.frame(t2)

t4<-do.call(rbind, lapply(names(t2[[1]]), function(u) transform(t2[[1]][[u]], type=u)))

t5<-sapply(t2, function(x) rbind.fill(Reduce(unlist, t2)))


w5b <- q2 %>% group_by(taxa) %>% do(model = as.data.frame(summary(aov(trig~d*abundance, data = .),)))


w6 <- q2 %>% group_by(taxa) %>% do(model = anova(lm(trig~d*abundance, data = .,),test="F"))
y1<-sapply(e2$model, function(x) x$"Pr(>F)")
y2<-t(as.data.frame(y1))


w5 <- q2 %>% group_by(taxa) %>% do(model = aov(trig~d*abundance, data = .))

r3<-sapply(w5$model, function(x) summary(x)[[1]][["Pr(>F)"]])

lapply(r3, write, "t.txt", append=TRUE, ncolumns=1000) 

t <- read_table2("t.txt", col_names = FALSE)
file.remove("t.txt")
w6<-as.data.frame(w5$taxa)
w7<- cbind(w6, t)



w11 <- q2 %>% group_by(taxa) %>% do(model = summary(lm(trig~d*abundance, data = .,),))
y4<-sapply(w11$model, function(x) x$"Pr(>F)")
y5<-t(as.data.frame(y4))


w11$model


w5 <- q1 %>% group_by(taxa) %>% do(model = aov(abundance~d*g*t, data = .)) # extract anova model without saving txt
r3<-sapply(w5$model, function(x) summary(x)[[1]][["Pr(>F)"]])
r4<-as.data.frame(t(r3))
r5<-as.data.frame(w5$taxa)
r6<-cbind(r4,r5)






lapply(split(df, df$Letter), aov, formula=Question ~ Number)
Alternatively using dplyr:
  
  library(dplyr)
obj <- df %>% group_by(Letter) %>% do(model = aov(Question~Number, data = .))
obj$model
Using data.table:
  
  library(data.table)
df <- as.data.table(df)
df[, list(Model = list(aov(Question ~ Number))), keyby = Letter]$Model
