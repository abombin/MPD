library(dplyr)



z1<-q[(q$DT=="PHFNS" |q$DT=="PHFS" |q$DT=="RHFNS"|q$DT=="RHFS"|q$DT=="RHSS"|q$DT=="RHSNS"),]
z2 <- z1 %>% group_by(Treatment) %>% do(model =TukeyHSD(aov(glm(trig~Diet,family=quasipoisson, data = .,))))
z3<-lapply(z2$model, function(x) unlist(x$"Diet"))
z4<-do.call(rbind, z3)
z5<-as.data.frame(z4)
