


w1<-all_d_zn10_f
w<- w1[, -c(1, 3:7) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(Diet ~ ., data = b)

iris.lda.values <- predict(iris.lda, b[,-1])

data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
rad <- 3 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad

#Plot the results
ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD1, x = LD2, colour = Diet)) + 
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = .3) +
  geom_point() +
  geom_hline(yintercept = 0, size = .2) + 
  geom_vline(xintercept = 0, size = .2) +
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
            data.lda, size = 4, vjust = .5, hjust = 0, colour = "red") +
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "red", radius = rad, size = 1) +
  ggtitle("LDA") +
  theme(legend.position = "right")





w1<-all_d_zn_f
q<- w1[(w1$Diet=="PR"|w1$Diet=="RHF"|w1$Diet=="PHF" ),]
w<- q[, -c(1, 3:7) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(Diet ~ ., data = b)

iris.lda.values <- predict(iris.lda, b[,-1])

data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
rad <- 2 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad

#Plot the results
ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD1, x = LD2, colour = Diet)) + 
  geom_point() +
  stat_conf_ellipse()+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = .3, level=0.95)+
  geom_hline(yintercept = 0, size = .2) + 
  geom_vline(xintercept = 0, size = .2) +
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
            data.lda, size = 4, vjust = .5, hjust = 0, colour = "red") +
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "red", radius = rad, size = 1) +
  ggtitle("LDA") +
  theme(legend.position = "right")



# all families
w1<-all_d_zn_f
w<- w1[, -c(1, 3:7) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(Diet ~ ., data = b)

iris.lda.values <- predict(iris.lda, b[,-1])

data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
rad <- 1.5 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad

#Plot the results with vectors and labels
ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD2, x = LD1, colour = Diet)) + 
  geom_point() +
  stat_conf_ellipse()+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = .3, level=0.5)+
  geom_hline(yintercept = 0, size = .2) + 
  geom_vline(xintercept = 0, size = .2) +
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = 0.1),
            data.lda, size = 4, vjust = .5, hjust = 0, colour = "black") +
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "black", radius = rad, size = 1) +
  ggtitle("LDA") +
  theme(legend.position = "right")+
  theme_light()

### castomized
ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD2, x = LD1, colour = Diet)) + 
  geom_point() +
  stat_conf_ellipse()+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = .3, level=0.5)+
  
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = 0.1),
            data.lda, size = 4, vjust = .5, hjust = 0, colour = "black") +
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "black", radius = rad, size = 1) +
  ggtitle("LDA") +
  theme(legend.position = "right")+
  theme_light()




ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD1, x = LD2, colour = Diet)) + 
  geom_point() +
  stat_conf_ellipse()+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = .3, level=0.95, )+
  geom_mark_ellipse(aes(label=Diet))+
  geom_hline(yintercept = 0, size = .2) + 
  geom_vline(xintercept = 0, size = .2) +

  
  ggtitle("LDA") +
  theme(legend.position = "right")


ggplot(cbind(b, iris.lda.values$x),
       aes(x = LD1, y = LD2, colour = Diet, label= Diet)) + 
  theme_light()+
  geom_point() +
  stat_ellipse(aes(color=Diet),geom = "polygon", alpha = .01, level=0.95 )+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  geom_hline(yintercept = 0, size = .2) + 
  geom_vline(xintercept = 0, size = .2) +
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = 0.1),
            data.lda, size = 4, vjust = .5, hjust = 0, colour = "red")+
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "red", radius = rad, size = 1) +
  ggtitle("LDA") +
  theme(legend.position = "right")
  


scale.para<-0.75
a<-ggplot(cbind(b, iris.lda.values$x),
       aes(x = LD1, y = LD2, colour = Diet, label= Diet)) + 
  theme_light()+
  geom_point() +
  stat_ellipse(aes(color=Diet),geom = "polygon", alpha = .01, level=0.95 )+
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  geom_hline(yintercept = 0, size = .2) + 
  geom_vline(xintercept = 0, size = .2) +
  ggtitle("LDA") +
  theme(legend.position = "right")



a1<-a+geom_segment(data=data.lda,
             aes(x=0, y=0,
                 xend=LD1*scale.para, yend=LD2*scale.para,
                 shape=NULL, linetype=NULL,
                 alpha=length),
             arrow=arrow(length=unit(0.2,"cm")),
             color="red")






w1<-all_d_zn_f
q<- w1[(w1$Diet=="PR"|w1$Diet=="RHF"|w1$Diet=="PHF" ),]
w<- q[, -c(1, 3:7) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(Diet ~ ., data = b)

iris.lda.values <- predict(iris.lda, b[,-1])

data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
rad <- 2 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad

#Plot the results
ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD1, x = LD2, colour = Diet)) + 
  geom_point() +
  stat_conf_ellipse()+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = .3, level=0.95)+
  geom_hline(yintercept = 0, size = .2) + 
  geom_vline(xintercept = 0, size = .2) +
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
            data.lda, size = 4, vjust = .5, hjust = 0, colour = "red") +
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length, radius=length), data.lda, 
             color = "red", size = 1) +
  ggtitle("LDA") +
  theme(legend.position = "right")





data.lda$l10<- data.lda$length*10

p <- qplot(data=data.frame(iris.lda.values$x),
           main="LDA",
           x=LD1,
           y=LD2,
           colour=b$Diet)+stat_ellipse(geom="polygon", alpha=.3, aes(fill=b$Diet))
p <- p + geom_hline(aes(yintercept=0), size=.2) + geom_vline(aes(xintercept=0), size=.2)
p <- p + theme(legend.position="right")
p <- p + geom_text(data=data.lda,
                   aes(x=LD1, y=LD2,
                       label=varnames, 
                       shape=NULL, linetype=NULL,
                       alpha=length, position="identity"),
                   size = 4, vjust=.5,
                   hjust=0, color="red")
p <- p + geom_segment(data=data.lda,
                      aes(x=0, y=0,
                          xend=LD1, yend=LD2,
                          shape=NULL, linetype=NULL,
                          alpha=length),
                      arrow=arrow(length=unit(1,"cm")),
                      color="red")
p <- p + coord_flip()
