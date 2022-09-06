library(vegan)
library(DescTools)

# Shannon
q1<-transpose_all_zotu_norm
q<- q1[(q1$Treatment!="food"),]
qa<- select(q, -c(Label, Group, "Genetic line", Treatment, Round, "Label 2", DT, Type, Sample, Diet))
r<-diversity(qa, index = "shannon", MARGIN = 1, base = exp(1))
x<-as.matrix(r)
x1<-as.data.frame(x)
x1$DT = q$DT
x1$Diet=q$Diet
x1$Treatment=q$Treatment
x1$Type=q$Type
colnames(x1)<-c("Shannon", "DT", "Diet", "Treatment", "Type")

x2<-x1[(x1$Type=="HF"),]
w5 <- x2 %>% group_by(Diet) %>% do(model = aov(Shannon~Treatment, data = .))
w1<-lapply(w5$model, function(x) PostHocTest(x, method="lsd"))
r3<-sapply(w1, function(x) x$Treatment)
r4<-as.data.frame(t(r3))
r5<-as.data.frame(r4[, -c(1:3) ])
y1<-as.data.frame(w5$Diet)
y2<-cbind(y1,r5)
colnames(y2)[1] <- "Diet"
colnames(y2)[2] <- "Shannon p"
u1<-y2

x2<-x1[(x1$Type=="HS"),]
w5 <- x2 %>% group_by(Diet) %>% do(model = aov(Shannon~Treatment, data = .))
w1<-lapply(w5$model, function(x) PostHocTest(x, method="lsd"))
r3<-sapply(w1, function(x) x$Treatment)
r4<-as.data.frame(t(r3))
r5<-as.data.frame(r4[, -c(1:3) ])
y1<-as.data.frame(w5$Diet)
y2<-cbind(y1,r5)
colnames(y2)[1] <- "Diet"
colnames(y2)[2] <- "Shannon p"
u2<-y2
u3<-rbind(u1,u2)
i1<-u3

#Simpson
r<-diversity(qa, index = "simpson", MARGIN = 1)
x<-as.matrix(r)
x1<-as.data.frame(x)
x1$DT = q$DT
x1$Diet=q$Diet
x1$Treatment=q$Treatment
x1$Type=q$Type
colnames(x1)<-c("Simpson", "DT", "Diet", "Treatment", "Type")

x2<-x1[(x1$Type=="HF"),]
w5 <- x2 %>% group_by(Diet) %>% do(model = aov(Simpson~Treatment, data = .))
w1<-lapply(w5$model, function(x) PostHocTest(x, method="lsd"))
r3<-sapply(w1, function(x) x$Treatment)
r4<-as.data.frame(t(r3))
r5<-as.data.frame(r4[, -c(1:3) ])
y1<-as.data.frame(w5$Diet)
y2<-cbind(y1,r5)
colnames(y2)[1] <- "Diet"
colnames(y2)[2] <- "Simpson p"
u1<-y2

x2<-x1[(x1$Type=="HS"),]
w5 <- x2 %>% group_by(Diet) %>% do(model = aov(Simpson~Treatment, data = .))
w1<-lapply(w5$model, function(x) PostHocTest(x, method="lsd"))
r3<-sapply(w1, function(x) x$Treatment)
r4<-as.data.frame(t(r3))
r5<-as.data.frame(r4[, -c(1:3) ])
y1<-as.data.frame(w5$Diet)
y2<-cbind(y1,r5)
colnames(y2)[1] <- "Diet"
colnames(y2)[2] <- "Simpson p"
u2<-y2
u3<-rbind(u1,u2)
i2<-u3

#Richness
r<-specnumber(qa, MARGIN = 1)
x<-as.matrix(r)
x1<-as.data.frame(x)
x1$DT = q$DT
x1$Diet=q$Diet
x1$Treatment=q$Treatment
x1$Type=q$Type
colnames(x1)<-c("Richness", "DT", "Diet", "Treatment", "Type")

x2<-x1[(x1$Type=="HF"),]
w5 <- x2 %>% group_by(Diet) %>% do(model = aov(Richness~Treatment, data = .))
w1<-lapply(w5$model, function(x) PostHocTest(x, method="lsd"))
r3<-sapply(w1, function(x) x$Treatment)
r4<-as.data.frame(t(r3))
r5<-as.data.frame(r4[, -c(1:3) ])
y1<-as.data.frame(w5$Diet)
y2<-cbind(y1,r5)
colnames(y2)[1] <- "Diet"
colnames(y2)[2] <- "Richness p"
u1<-y2

x2<-x1[(x1$Type=="HS"),]
w5 <- x2 %>% group_by(Diet) %>% do(model = aov(Richness~Treatment, data = .))
w1<-lapply(w5$model, function(x) PostHocTest(x, method="lsd"))
r3<-sapply(w1, function(x) x$Treatment)
r4<-as.data.frame(t(r3))
r5<-as.data.frame(r4[, -c(1:3) ])
y1<-as.data.frame(w5$Diet)
y2<-cbind(y1,r5)
colnames(y2)[1] <- "Diet"
colnames(y2)[2] <- "Richness p"
u2<-y2
u3<-rbind(u1,u2)
i3<-u3

i4<-cbind(i1,i2,i3)
i5<-i4[, -c(3,5) ]
i5$Comparison<-"NS vs S"

write.csv(i5, "hfhs_all_zn_alpd_t.csv")




