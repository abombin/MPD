library(ggplot2)
library(dplyr)
library(MASS)
library(ggpubr)


q1a<-hfhs_zn10_s
#HF NS
q1<-q1a[!(q1a$"type"=="food"),]
q1$Diet <- gsub("13", "11", q1$Diet)
q2<- as.data.frame(paste(q1$Diet, q1$Treatment, sep=""))
q<-cbind(q2,q1)
colnames(q)[1] <- "DT"
#HF NS
w1<-q[(q$"type"=="HF"& q$"Treatment"=="NS"),]
w<- w1[, -c(2:7) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(DT ~ ., data = b)
iris.lda.values <- predict(iris.lda, b[,-1])
data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
rad <- 1.5 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad
#Plot the results with vectors
ggplot(cbind(b, iris.lda.values$x),
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
### save
ggsave("disc_hf_zn_d_ns_100.png", dpi = 100, width=25, height=25, units= c("cm"))
ggsave("disc_hf_zn_d_ns_300.png", dpi = 300, width=25, height=25, units= c("cm"))  
#HF S
w1<-q[(q$"type"=="HF"& q$"Treatment"=="S"),]
w<- w1[, -c(2:7) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(DT ~ ., data = b)
iris.lda.values <- predict(iris.lda, b[,-1])
data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
rad <- 1.5 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad
#Plot the results with vectors
ggplot(cbind(b, iris.lda.values$x),
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
### save
ggsave("disc_hf_zn_d_s_100.png", dpi = 100, width=25, height=25, units= c("cm"))
ggsave("disc_hf_zn_d_s_300.png", dpi = 300, width=25, height=25, units= c("cm"))  

#HS NS
w1<-q[(q$"type"=="HS"& q$"Treatment"=="NS"),]
w<- w1[, -c(2:7) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(DT ~ ., data = b)
iris.lda.values <- predict(iris.lda, b[,-1])
data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
rad <- 1.5 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad
#Plot the results with vectors
ggplot(cbind(b, iris.lda.values$x),
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
### save
ggsave("disc_hs_zn_d_ns_100.png", dpi = 100, width=25, height=25, units= c("cm"))
ggsave("disc_hs_zn_d_ns_300.png", dpi = 300, width=25, height=25, units= c("cm"))

#HS S
w1<-q[(q$"type"=="HS"& q$"Treatment"=="S"),]
w2<-w1[!(w1$DT=="PHSA11S"),]
w<- w2[, -c(2:7) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(DT ~ ., data = b)
iris.lda.values <- predict(iris.lda, b[,-1])
data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
rad <- 1.5 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad
#Plot the results with vectors
ggplot(cbind(b, iris.lda.values$x),
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
### save
ggsave("disc_hs_zn_d_s_100.png", dpi = 100, width=25, height=25, units= c("cm"))
ggsave("disc_hs_zn_d_s_300.png", dpi = 300, width=25, height=25, units= c("cm"))


