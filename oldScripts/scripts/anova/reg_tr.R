w5 <- q1 %>% group_by(taxa) %>% do(model = anova(trig~d*abundance, data = .,))

r3<-sapply(w5$model, function(x) summary(x)[[1]][["Pr(>F)"]])


a4<- glm(trig~abundance*d, family=quasipoisson, data=q1)

pv <- as.data.frame(summary(a4)$coef[, "Pr(>|t|)"])

a5<-anova(a4, test="F")

a6a<-AIC(w4a, k=2) #works with linear models but not glm
a7<-extractAIC(a4)
a7a<-extractAIC(w4a)
              

e2 <- q1 %>% group_by(taxa) %>% do(model = anova(glm(trig~d*abundance,family=quasipoisson, data = .,),test="F"))  #automatic glm model
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))

e2v <- q1 %>% group_by(taxa) %>% do(model=glm(trig~d*abundance,family=quasipoisson, data = .,))
e2v1<-lapply(e2v$model, function(x) summary(x,))
new_list <- plyr::adply(e2v1,1,unlist,.id = NULL)
lapply(e2v1, function(x) write (print(x), "x1.csv", append = TRUE))
library(utils)
x6<-as.data.frame(capture.output(print(e2v1)))

e4<-lapply(e3, function(x) x$"Pr(>F)") #best function to extract elements from list of lists

a5a<-anova(a4, test="F", contrasts=list(IV=contr.poly), type="III") #does not work

a6<-lm(trig ~ d * t, data=q1, contrasts=list(d=contr.sum, t=contr.sum)) # does not work
a7<-Anova(a6, type="III") #does not work


e2 <- q1 %>% group_by(taxa) %>% do(model = summary(glm(trig~d*abundance,family=quasipoisson, data = .,),))  
u1<- sapply(e2$model, function(x) as.data.frame(rbind(x)))
e3<-sapply(e2$model, function(x) x$"Pr(>F)")
e4<-as.data.frame(t(e3))


m1 <- glm.nb(trig ~ abundance*d, data = q1)
m2<-anova(m1, test="F")
m3<-anova(m1, test="Chisq")
a5a<-anova(a4, test="Chisq")


m4 <- glm.nb(gluc ~ abundance*d, data = q1)
m5<-anova(m4, test="F")


a6<- glm(gluc~abundance*d, family=quasipoisson, data=q1)
a6a<-anova(a6, test="F")


e2a <- q1 %>% group_by(taxa) %>% do(model = anova(glm.nb(trig~d*abundance, data = .,),test="F"))  #automatic glm model
e3a<-sapply(e2a$model, function(x) x$"Pr(>F)")
e4a<-as.data.frame(t(e3a))



e2c <- q1 %>% group_by(taxa) %>% do(model = Anova(glm(trig~d*abundance,family=quasipoisson, data = .,),type="III",test.statistic ="F",singular.ok = TRUE))  #automatic glm model
e3c<-sapply(e2c$model, function(x) x$"Pr(>F)")
e4c<-as.data.frame(t(e3c))

e2c <- q1 %>% group_by(taxa) %>% do(model=glm(trig~d*abundance,family=quasipoisson, data = .,))
e2c1<-sapply(e2c$model, function(x) Anova(x, type="III", test.statistic = "F"))


z1<-Anova(a4, type=c("II", "III"), test.statistic=c("LR", "Wald", "F"))


library(MASS)

a1<- q1 %>% group_by(taxa) %>% do(model = anova(glm.nb(abundance~d*g*t, data = .,),test="F"))