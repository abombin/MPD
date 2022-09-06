# trial version
a<-hfhs_100zotu_norm
b<- select(a, -c(Label))
r<-diversity(b, index = "shannon", MARGIN = 1, base = exp(1))
x<-as.matrix(r)
x1<-as.data.frame(x)
x1$Label = a$Label
colnames(x1)<-c("Shannon", "Label")
# summary per category
q2<-as.data.frame(tapply(diversity(qa, "simpson"), q$DT, mean))
q3<- setDT(q2, keep.rownames = TRUE)[]

# real version
library(DescTools)
library(vegan)
library(utils)
#Shannon

q1<-transpose_100_zotu_unmod_norm
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

w5 <- x1 %>% group_by(Type,Treatment) %>% do(model = aov(Shannon~Diet, data = .))
w1<-lapply(w5$model, function(x) PostHocTest(x, method="lsd"))
x6<-as.data.frame(capture.output(print(w1)))

r3<-sapply(w5$model, function(x) summary(x)[[1]][["Pr(>F)"]])
r4<-as.data.frame(t(r3))
r5<-as.data.frame(w5$Treatment)
r6<-cbind(r4,r5)






