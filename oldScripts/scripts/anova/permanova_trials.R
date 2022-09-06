library(vegan)

adonis2(dune ~ Management*A1, data = dune.env)

w6<-lapply(split(q1, q1$taxa), FUN=adonis(trig~abundance*d, data=q1, na.rm=TRUE, permutations=5))

w7<- adonis(trig~abundance*d, data=q1, na.rm=TRUE, permutations=5) #does not work that far
w8a<- adonis(trig~d, data=q2, na.rm=TRUE, permutations=5)# works only if NA removed from the column for tested effect

q2<-q1[!is.na(q1$trig), ]
w9<- adonis(trig ~ d*abundance, permutations=5, data = q2)


library(dplyr)
obj <- q1 %>% group_by(taxa) %>% do(model = aov(trig~d*abundance, data = .))

r3b<-sapply(obj$model, function(x) summary(x)[[1]][["Pr(>F)"]])

a3<-as.data.frame(r3b$value)


library(RVAideMemoire)

w1<- perm.anova(trig~d,  data=q1, nperm = 999, progress = TRUE) # works only with a single variable


w5 <- q1 %>% group_by(taxa) %>% do(model = perm.anova(trig~d,  data=q1, nperm = 999, progress = TRUE))
w2 <- q1 %>% group_by(taxa) %>% do(model = adonis(trig ~ d*abundance, permutations=5, data = q2))
r3<-sapply(w2$model, function(x) summary(x)[[1]][["Pr(>F)"]])

w9<- adonis(trig ~ d*abundance, permutations=5, data = q2)



r3<-sapply(w5$model, function(x) summary(x)[[1]][["Pr(>F)"]])
r4<-sapply(w5, FUN=duplicate(w5$model))

lapply(r3, write, "t.txt", append=TRUE, ncolumns=1000) 

t <- read_table2("t.txt", col_names = FALSE)
file.remove("t.txt")

w6<-as.data.frame(w5$taxa)
w7<- cbind(w6, t)


r5<- w5 %>% duplicate(w5$model)


do <- as.data.frame(do.call(rbind, lapply(w2, as.vector)))
do1<-t(do)
r2<-select(w2$model$"Pr(>F)")
r4<-as.data.frame(paste(w5$model))
colnames(r4)[colnames(r4)=="paste(w5$model)"] <- "a"
r5<-as.data.frame(sub(".*Pr(>F)","",r4$a))
r4a<-as.data.frame(paste(r4$model$"Pr(>F)"))
r5a<-as.character(r4$a)
r5b<-as.data.frame(r5a)

r8<-as.data.frame(r7)

r7a<- sub(".*?=", "", r4$a)
r8a<-gsub("^.*\\.","", data)

r9<-gsub('(.*) 'Pr(>F)':','', r4$a)

r10<- paste(r6$model$"Pr(>F)")


e1<-as.data.frame(as.character(r4$a))
e2<-as.data.frame(as_string(r4$a))


write.csv(r4, "r4.csv")
my_names="Pr(>F)"
result = lapply(w5, [, , my_names)
r11<-  lapply(w5, function(x) paste(w5$model[model$"Pr(>F)"]))
new_list <- lapply(w5, function(x) w5$model%>% select("Pr(>F)"))

new_list <- lapply(r6, function(x) r6$model%>% select("Pr(>F)"))


t1<-list.select(w5, model$"Pr(>F)")

t2<-bind_rows(w5, .id = "Pr(>F)")


a3<- as.data.frame(unlist(w5$model)) #somewhat working if every row will be consistent across all the models

a4<-gsub('(.*)`Pr(>F)`','', r4$a)

foo <- data.frame(do.call('rbind', strsplit(as.character(r4$a),' ',fixed=TRUE))) # the most convinient way if all elements are at their place every time





a1<-w2$model
r3<-lapply(a1, function(x) summary(x)[[1,]])

a2<-lapply(w2$model, FUN=unlist)


e5<-lapply(a1, function(x) x$"aov.tab")
e6<- sapply(e5, function(x) x$"Pr(>F)")
e7<-t(as.data.frame(e6))


e8<-lapply(w2$model, function(x) x$"aov.tab") # best function to extract p values from adonis