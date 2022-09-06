library(multcomp)
model<-glm(trig~Diet,family=quasipoisson, data = q)

b<-anova(a, test = "F")
c<- glht(model, mcp(Diet = "Tukey"))
d<-TukeyHSD(aov(a))
q1<-q[(q$DT=="PHFNS" |q$DT=="PHFS" |q$DT=="RHFNS"|q$DT=="RHFS"|q$DT=="RHSS"|q$DT=="RHSNS"),]
f2 <- q1 %>% group_by(Treatment) %>% do(model =TukeyHSD(aov(glm(trig~Diet,family=quasipoisson, data = .,))))
e3<-lapply(f2$model, function(x) unlist(x$"Diet"))
e4<-sapply(e3, function(x) x$"p adj")
e4<-as.data.frame(unlist(e3))

dd  <-  as.data.frame(matrix(unlist(e3), nrow=length(unlist(e3[1]))))

dd1  <-  sapply(e3, function(x) as.data.frame(matrix(unlist(x), nrow=length(unlist(x[1])))))

a1<-e3[[1]]
a2<- do.call(rbind, e3)

