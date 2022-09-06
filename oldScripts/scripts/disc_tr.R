library(MASS)
library(ggplot2)
library(ggforce)
library(dplyr)
w1<-all_d_zn_f
# prepare table
q<- w1
w<- q[ -c(1,3:7) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
m.lda <- lda(Diet ~ ., data = b)
# preparing a plot
m.sub <- b %>% dplyr::select(-Diet) %>% as.matrix  
CVA.scores <- m.sub %*% m.lda$scaling
m.CV <- data.frame(CVA.scores)
m.CV$Diet <- b$Diet
# making plot
m.cva.plot <-
  ggplot(m.CV, aes(x = LD1, y = LD2)) + 
  geom_point(aes(color=Diet), alpha=0.5) + 
  labs(x = "CV1", y = "CV2") +
  coord_fixed(ratio=1) 
m.cva.plot
# confidence elipses 
chi2 = qchisq(0.05,2, lower.tail=FALSE)
CIregions.mean.and.pop <-
  m.CV %>%
  group_by(Diet) %>%
  summarize(CV1.mean = mean(LD1),
            CV2.mean = mean(LD2),
            mean.radii = sqrt(chi2/n()),
            popn.radii = sqrt(chi2))
# plot with elipses
m.cva.plot2 <-
  m.cva.plot + 
  geom_circle(data = CIregions.mean.and.pop,
              mapping = aes(x0 = CV1.mean, y0 = CV2.mean, r = mean.radii),
              inherit.aes = FALSE) +
  geom_circle(data = CIregions.mean.and.pop,
              mapping = aes(x0 = CV1.mean, y0 = CV2.mean, r = popn.radii),
              linetype = "dashed", 
              inherit.aes = FALSE) 

m.cva.plot2







w1<-all_d_zn_f
# prepare table
q<- w1
w<- q[ -c(1,3:7) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
m.lda <- lda(Diet ~ ., data = b)
# preparing a plot
m.sub <- b %>% dplyr::select(-Diet) %>% as.matrix  
CVA.scores <- m.sub %*% m.lda$scaling
m.CV <- data.frame(CVA.scores)
m.CV$Diet <- b$Diet
# making plot
m.cva.plot <-
  ggplot(m.CV, aes(x = LD1, y = LD2)) + 
  geom_point(aes(color=Diet)) +
  labs(x = "CV1", y = "CV2") + coord_fixed(ratio=1) + theme_light()   
m.cva.plot

# confidence elipses 
chi2 = qchisq(0.05,2, lower.tail=FALSE)
CIregions.mean.and.pop <-
  m.CV %>%
  group_by(Diet) %>%
  summarize(CV1.mean = mean(LD1),
            CV2.mean = mean(LD2),
            mean.radii = sqrt(chi2/n()),
            popn.radii = sqrt(chi2))
# plot with elipses
m.cva.plot2 <-
  m.cva.plot + 
  geom_circle(data = CIregions.mean.and.pop,
              mapping = aes(x0 = CV1.mean, y0 = CV2.mean, r = mean.radii),
              inherit.aes = FALSE )  +
  geom_circle(data = CIregions.mean.and.pop,
              mapping = aes(x0 = CV1.mean, y0 = CV2.mean, r = popn.radii),
               
              inherit.aes = FALSE) +
  geom_label(data = CIregions.mean.and.pop,
             mapping = aes(x = CV1.mean, 
                           y = CV2.mean + popn.radii, 
                           label = Diet),
             label.padding = unit(0.1, "lines"),
             label.size = 0)
  
m.cva.plot2




#hfhs
w1<-hfhs_trans_zotu_normalized_sum_family
# prepare table
q<- w1
w<- q[, -c(1, 3:10) ]
b<- w[c(rep(TRUE, 1L), colSums(w[2L:ncol(w)]) > 1L)]
# linear discriminant
m.lda <- lda(Diet ~ ., data = b)
# preparing a plot
m.sub <- b %>% dplyr::select(-Diet) %>% as.matrix  
CVA.scores <- m.sub %*% m.lda$scaling
m.CV <- data.frame(CVA.scores)
m.CV$Diet <- b$Diet
# making plot
m.cva.plot <-
  ggplot(m.CV, aes(x = LD1, y = LD2)) + 
  geom_point(aes(color=Diet)) +
  labs(x = "CV1", y = "CV2") + coord_fixed(ratio=1) + theme_light()   
m.cva.plot

# confidence elipses 
chi2 = qchisq(0.05,2, lower.tail=FALSE)
CIregions.mean.and.pop <-
  m.CV %>%
  group_by(Diet) %>%
  summarize(CV1.mean = mean(LD1),
            CV2.mean = mean(LD2),
            mean.radii = sqrt(chi2/n()),
            popn.radii = sqrt(chi2))
# plot with elipses
m.cva.plot2 <-
  m.cva.plot + 
  geom_circle(data = CIregions.mean.and.pop,
              mapping = aes(x0 = CV1.mean, y0 = CV2.mean, r = mean.radii),
              inherit.aes = FALSE )  +
  geom_circle(data = CIregions.mean.and.pop,
              mapping = aes(x0 = CV1.mean, y0 = CV2.mean, r = popn.radii),
              
              inherit.aes = FALSE) +
  geom_label(data = CIregions.mean.and.pop,
             mapping = aes(x = CV1.mean, 
                           y = CV2.mean + popn.radii, 
                           label = Diet),
             label.padding = unit(0.1, "lines"),
             label.size = 0)

m.cva.plot2

rad <- 3 # This sets the length of your lines.
m.CV$length <- with(m.CV, sqrt(LD1^2+LD2^2))
m.CV$angle <- atan2(m.CV$LD1, m.CV$LD2)
m.CV$x_start <- m.CV$y_start <- 0
m.CV$x_end <- cos(m.CV$angle) * rad
m.CV$y_end <- sin(m.CV$angle) * rad

m.cva.plor3<- m.cva.plot2+
  geom_spoke(aes(x_start, y_start, angle = angle, alpha = length), data.lda, 
             color = "red", radius = rad, size = 1)+
  geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
            data.lda, size = 4, vjust = .5, hjust = 0, colour = "red")
  

m.cva.plor3

rad <- 3 # This sets the length of your lines.
data.lda$length <- with(data.lda, sqrt(LD1^2+LD2^2))
data.lda$angle <- atan2(data.lda$LD1, data.lda$LD2)
data.lda$x_start <- data.lda$y_start <- 0
data.lda$x_end <- cos(data.lda$angle) * rad
data.lda$y_end <- sin(data.lda$angle) * rad


data.lda <- data.frame(varnames=rownames(coef(m.lda)), coef(m.lda))

m.CV$varnames<-rownames(coef(m.lda))

geom_text(aes(y = y_end, x = x_end, label = varnames, alpha = length),
          m.CV, size = 4, vjust = .5, hjust = 0, colour = "red")