library(ggplot2)
library(dplyr)
library(MASS)
library(ggpubr)


q <- read_delim("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_zn100_z.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE) ### 100 zotu
q<-as.data.frame(q)



q$Diet <- gsub("13", "11", q$Diet)
q$Diet <- gsub("PHS11", "PHS", q$Diet)
q$Diet <- factor(q$Diet, level=c("R", "RHF", "RHS", "PR", "PHF", "PHS", "PA", "PHFA", "PHSA6", "PHSA11"))
DT<-as.data.frame(paste(q$Diet, q$Treatment, sep=""))
q1<-cbind(DT, q)
colnames(q1)[1]<-"DT"

# 
a<- q1[(q1$"DT"=="RHFNS" |q1$"DT"=="RHFS" | q1$"DT"=="RHSNS" | q1$"DT"=="RHSS" | q1$"DT"=="RNS" | q1$"DT"=="RS"),]
w<- a[, -c(2:8) ]

b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(DT ~ ., data = b)
iris.lda.values <- predict(iris.lda, b[,-1])
data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))

# plot
group.colors <- c("R" = "#F86766D", "RHF" = "#7CAE00", "RHS" ="00BFC4")
ggplot(cbind(a, iris.lda.values$x),
       aes(y = LD1, x = LD2, colour = Diet)) + 
  geom_point(aes(shape=Treatment, color=Diet), size=5) +
  stat_conf_ellipse(aes(color = DT), alpha = 0.3, geom = "polygon", show.legend=FALSE)+
  stat_ellipse(aes(fill = DT), geom = "polygon", alpha = 0.01, level=0.95, show.legend = FALSE)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 44))+
  guides(shape = guide_legend(order = 1))
 
#coord_cartesian(xlim = c(-3, 6))
ggsave("alld_R_disc_100z_norm_alldt1.png", dpi = 300, width=25, height=25, units= c("cm"))

# 
a<- q1[(q1$"DT"=="PHFNS" |q1$"DT"=="PHFS" | q1$"DT"=="PHSNS" | q1$"DT"=="PHSS" | q1$"DT"=="PRNS" | q1$"DT"=="PRS"),]
w<- a[, -c(2:8) ]

b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
iris.lda <- lda(DT ~ ., data = b)
iris.lda.values <- predict(iris.lda, b[,-1])
data.lda <- data.frame(varnames=rownames(coef(iris.lda)), coef(iris.lda))

# plot
group.colors <- c("PR" = "#F86766D", "PHF" = "#7CAE00", "PHS" ="00BFC4")

ggplot(cbind(a, iris.lda.values$x),
       aes(y = LD1, x = LD2, colour = Diet)) + 
  geom_point(aes(shape=Treatment, color=Diet), size=5) +
  stat_conf_ellipse(aes(color = DT), alpha = 0.3, geom = "polygon", show.legend=FALSE)+
  stat_ellipse(aes(fill = DT), geom = "polygon", alpha = 0.01, level=0.95, show.legend = FALSE)+
  
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme(text = element_text(size = 44))

#coord_cartesian(xlim = c(-3, 6))
ggsave("alld_P_disc_100z_norm_alldt.png", dpi = 300, width=25, height=25, units= c("cm"))
