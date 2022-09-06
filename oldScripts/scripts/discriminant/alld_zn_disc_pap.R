library(ggplot2)
library(dplyr)
library(MASS)
library(ggpubr)

q1a <- read_delim("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_zn10_f.txt", 
                           "\t", escape_double = FALSE, col_types = cols(Genotype = col_character(), 
                                                                         Round = col_character()), trim_ws = TRUE)


q1<-q1a[!(q1a$"type"=="food"),]
q1$Diet <- gsub("13", "11", q1$Diet)
q1$Diet <- gsub("PHS11", "PHS", q1$Diet)
q2<- as.data.frame(paste(q1$Diet, q1$Treatment, sep=""))
q<-cbind(q2,q1)
colnames(q)[1] <- "DT"
#R NS
w1<-q[(q$DT=="RNS"|q$DT=="RHFNS"|q$DT=="RHSNS"),]
w<- w1[, -c(1:2, 4:8) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(Diet ~ ., data = b)
iris.lda.values <- predict(iris.lda, b[,-1])
data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
rad <- 1.5 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad

b$Diet <- factor(b$Diet, level=c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11"))

#Plot the results with vectors
ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD1, x = LD2, colour = Diet)) + 
  geom_point(aes(shape=Diet, color=Diet), size=5) +
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
            data.lda, size = 10, vjust = .5, hjust = 0, colour = "black", show.legend = FALSE) +
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "black", radius = rad, size = 1, show.legend = FALSE) +
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 44))+
  coord_cartesian(xlim = c(-3, 6))
### save
ggsave("disc_alld_zn_r_ns_300_pap.png", dpi = 300, width=25, height=25, units= c("cm")) 

#R S
w1<-q[(q$DT=="RS"|q$DT=="RHFS"|q$DT=="RHSS"),]
w<- w1[, -c(1:2, 4:8) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(Diet ~ ., data = b)
iris.lda.values <- predict(iris.lda, b[,-1])
data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
rad <- 1.5 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad

b$Diet <- factor(b$Diet, level=c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11"))

#Plot the results with vectors
ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD1, x = LD2, colour = Diet)) + 
  geom_point(aes(shape=Diet, color=Diet), size=5) +
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
            data.lda, size = 10, vjust = .5, hjust = 0, colour = "black", show.legend = FALSE) +
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "black", radius = rad, size = 1, show.legend = FALSE) +
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 44))+
  coord_cartesian(xlim = c(-3, 6))
### save
ggsave("disc_alld_zn_r_s_300_pap.png", dpi = 300, width=25, height=25, units= c("cm")) 

# PR NS
w1<-q[(q$DT=="PRNS"|q$DT=="PHFNS"|q$DT=="PHSNS"),]
w<- w1[, -c(1:2, 4:8) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(Diet ~ ., data = b)
iris.lda.values <- predict(iris.lda, b[,-1])
data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
rad <- 1.5 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad

b$Diet <- factor(b$Diet, level=c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11"))

#Plot the results with vectors
ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD1, x = LD2, colour = Diet)) + 
  geom_point(aes(shape=Diet, color=Diet), size=5) +
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
            data.lda, size = 10, vjust = .5, hjust = 0, colour = "black", show.legend = FALSE) +
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "black", radius = rad, size = 1, show.legend = FALSE) +
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 44))+
  coord_cartesian(xlim = c(-3, 6))
### save
ggsave("disc_alld_zn_p_ns_300_pap.png", dpi = 300, width=25, height=25, units= c("cm")) 

# PR S
w1<-q[(q$DT=="PRS"|q$DT=="PHFS"|q$DT=="PHSS"),]
w<- w1[, -c(1:2, 4:8) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(Diet ~ ., data = b)
iris.lda.values <- predict(iris.lda, b[,-1])
data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
rad <- 1.5 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad

b$Diet <- factor(b$Diet, level=c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11"))

#Plot the results with vectors
ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD1, x = LD2, colour = Diet)) + 
  geom_point(aes(shape=Diet, color=Diet), size=5) +
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
            data.lda, size = 10, vjust = .5, hjust = 0, colour = "black", show.legend = FALSE) +
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "black", radius = rad, size = 1, show.legend = FALSE) +
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 44))+
  coord_cartesian(xlim = c(-3, 6))
### save
ggsave("disc_alld_zn_p_s_300_pap.png", dpi = 300, width=25, height=25, units= c("cm")) 


# PA NS
group.colors <- c("PA" = "#F8766D", "PHFA"= "#00BA38", "PHSA6"= "#619CFF", "PHSA11"="#E76BF3")

w1<-q[(q$DT=="PANS"|q$DT=="PHFANS"|q$DT=="PHSA6NS"| q$DT=="PHSA11NS"),]
w<- w1[, -c(1:2, 4:8) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(Diet ~ ., data = b)
iris.lda.values <- predict(iris.lda, b[,-1])
data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
rad <- 1.5 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad

b$Diet <- factor(b$Diet, level=c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11"))

#Plot the results with vectors
ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD1, x = LD2, colour = Diet)) + 
  geom_point(aes(shape=Diet, color=Diet), size=5) +
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  scale_fill_manual(values = group.colors)+
  scale_color_manual(values = group.colors)+
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
            data.lda, size = 10, vjust = .5, hjust = 0, colour = "black", show.legend = FALSE) +
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "black", radius = rad, size = 1, show.legend = FALSE) +
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 44))+
  coord_cartesian(xlim = c(-3, 8))
### save
ggsave("disc_alld_zn_pa_ns_300_pap.png", dpi = 300, width=25, height=25, units= c("cm")) 

#PA S
w1<-q[(q$DT=="PAS"|q$DT=="PHFAS"|q$DT=="PHSA6S"),]
w<- w1[, -c(1:2, 4:8) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(Diet ~ ., data = b)
iris.lda.values <- predict(iris.lda, b[,-1])
data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))
rad <- 1.5 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad

b$Diet <- factor(b$Diet, level=c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11"))

#Plot the results with vectors
ggplot(cbind(b, iris.lda.values$x),
       aes(y = LD1, x = LD2, colour = Diet)) + 
  geom_point(aes(shape=Diet, color=Diet), size=5) +
  stat_conf_ellipse(aes(color = Diet, fill = Diet), alpha = 0.3, geom = "polygon")+
  stat_ellipse(aes(fill = Diet), geom = "polygon", alpha = 0.01, level=0.5)+
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
            data.lda, size = 10, vjust = .5, hjust = 0, colour = "black", show.legend = FALSE) +
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "black", radius = rad, size = 1, show.legend = FALSE) +
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 44))+
  coord_cartesian(xlim = c(-3, 8))
### save
ggsave("disc_alld_zn_pa_s_300_pap.png", dpi = 300, width=25, height=25, units= c("cm")) 