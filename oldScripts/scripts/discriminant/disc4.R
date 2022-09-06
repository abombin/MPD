library(ggplot2)
library(dplyr)
library(MASS)
library(ggpubr)

q<-hfhs_trans_zotu_normalized_sum_phylum

w1<-q[(q$"Type"=="HF"),]
w<- w1[, -c(1:8, 10) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(DT ~ ., data = b)

iris.lda.values <- predict(iris.lda, b[,-1])

data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
rad <- 3 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad

#Plot the results with vectors
ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD2, x = LD1, colour = DT)) + 
  geom_point() +
  stat_conf_ellipse(aes(color = DT, fill = DT), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = DT), geom = "polygon", alpha = 0.01, level=0.5)+
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
            data.lda, size = 4, vjust = .5, hjust = 0, colour = "black") +
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "black", radius = rad, size = 1) +
  ggtitle("LDA") +
  theme(legend.position = "right")+
  theme_light()


#Plot the results without vectors
ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD2, x = LD1, colour = DT)) + 
  geom_point() +
  stat_conf_ellipse()+
  stat_ellipse(aes(fill = DT), geom = "polygon", alpha = 0.01, level=0.5)+
  ggtitle("LDA") +
  theme(legend.position = "right")+
  theme_light()



#Plot the results without vectors Diet color shape treatment
ggplot(cbind(w1, iris.lda.values$x),
       aes(y = LD2, x = LD1, colour = Diet, shape=Treatment)) + 
  geom_point() +
  stat_conf_ellipse()+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = .3, level=0.5)+
  ggtitle("LDA") +
  theme(legend.position = "right")+
  theme_light()


#HS 
w1<-q
w1<-q[(q$"Type"=="HS"),]
w2<-w1[!(w1$"DT"=="PHSA13S"),]
w<- w2[, -c(1:8, 10) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(DT ~ ., data = b)

iris.lda.values <- predict(iris.lda, b[,-1])

data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
rad <- 3 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad

ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD2, x = LD1, colour = DT)) + 
  geom_point() +
  stat_ellipse(aes(fill = DT), geom = "polygon", alpha = 0.01, level=0.5)+
  stat_conf_ellipse(aes(color = DT, fill = DT), alpha = 0.3, geom = "polygon")+
  ggtitle("LDA") +
  theme(legend.position = "right")+
  theme_light()




# only clustered HS

#HS 
w1<-q
w1<-q[(q$"Type"=="HS"),]
w2<-w1[(w1$"DT"=="RHSNS"|w1$"DT"=="PHSA13NS"|w1$"DT"=="PHSA6NS"|w1$"DT"=="PHSA6S"),]
w<- w2[, -c(1:8, 10) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(DT ~ ., data = b)

iris.lda.values <- predict(iris.lda, b[,-1])

data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
rad <- 3 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad

ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD2, x = LD1, colour = DT)) + 
  geom_point() +
  stat_ellipse(aes(fill = DT), geom = "polygon", alpha = 0.01, level=0.5)+
  stat_conf_ellipse(aes(color = DT, fill = DT), alpha = 0.3, geom = "polygon")+
  ggtitle("LDA") +
  theme(legend.position = "right")+
  theme_light()

