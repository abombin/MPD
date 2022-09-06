
q1a<-all_d_zn10_g

q1<-q1a[!(q1a$"type"=="food"),]
q1$Diet <- gsub("13", "11", q1$Diet)
q2<- as.data.frame(paste(q1$Diet, q1$Treatment, sep=""))
q<-cbind(q2,q1)
colnames(q)[1] <- "DT"
#R NS
w1<-q[(q$DT=="RNS"|q$DT=="RHFNS"),]
w<- w1[, -c(2:7) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(DT ~ ., data = b)
iris.lda.values <- predict(iris.lda, b[,-1])
data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
data.lda$LDA<-log10(data.lda$LD1)

data.lda$length <- with(data.lda, sqrt(LD1^2))

p<-ggplot(data=data.lda, aes(x=varnames, y=LD1)) +
  geom_bar(stat="identity")
p + coord_flip()

ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD1, x = LD1, colour = DT)) + 
  geom_point()+
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
            data.lda, size = 4, vjust = .5, hjust = 0, colour = "black", check_overlap = TRUE)
###



####
data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
data.lda$LD2<-data.lda$LD1
rad <- 1.5 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad
iris.ld<-as.data.frame(iris.lda.values$x)
iris.ld$LD2<-iris.ld$LD1
#Plot the results with vectors
ggplot(cbind(b, iris.ld),
       aes(y = LD1, x = LD2, colour = DT)) + 
  geom_point() +
  stat_conf_ellipse(aes(color = DT, fill = DT), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = DT), geom = "polygon", alpha = 0.01, level=0.5)+
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
            data.lda, size = 4, vjust = .5, hjust = 0, colour = "black") +
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "black", radius = rad, size = 1) +
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 18))



ggplot(cbind(b, iris.ld),
       aes(y = LD1, x = LD2, colour = DT)) + 
  geom_point() +
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "black", radius = rad, size = 1)+
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
            data.lda, size = 4, vjust = .5, hjust = 0, colour = "black")

