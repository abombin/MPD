

q<-R_and_HF_HS_trig_no_food
w<- q[(q$"DTG"=="PHSA6S748" ),]
e<- split(w$trig_l_weigh, sample( 3, replace = TRUE))
set.seed(1)
e1<-split(w$trig_l_weigh, sample(1:3, 100, replace = TRUE))


ss <- sample(1:3,size=nrow(w),replace=TRUE,prob=c(0.4,0.3,0.3))

mycars <- setNames(split(w$trig_l_weigh,ss), c("train","test","cvr"))



w$trig_l_weigh <- sample(factor(rep(1:3, length.out=nrow(w)), labels=paste0("Project", 1:3)))
dat <- data.frame(replicate(10, sample(0:1, 9558, rep=TRUE)))

e2<-split(w$trig_l_weigh, sample(rep(1:3, c(2))))

#best sequence that far
q<-R_and_HF_HS_trig_no_food
w<- q[(q$"DTG"=="PHSA13S748" ),]
e2<-split(w$trig_l_weigh, sample(rep(1:3, c(2))))