library(readr)
library(readxl)
library(readr)

z_n_phen_rand_s <- read.csv("C:/Users/abomb/Box/HF and HS/R/HF HS/input/z_n_phen_rand_s.csv") # txt, csv
R_and_HF_HS_collection_total <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/R and HF, HS collection total.xlsx") # xlsx
# joining tables for round phenotype
trig<-read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/R and HF,HS trig no food.xlsx")
gluc<-read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/R and HF,HS gluc no food.xlsx")
prot<-read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/R and HF,HS prot no food.xlsx")
# uodating microbiota tables

t1<-read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/transpose_all_zotu_norm.xlsx")
#spearman round

z_n_phen_round_z <- read.csv("C:/Users/abomb/Box/HF and HS/R/HF HS/input/z_n_phen_round_z.csv")
#spearman randomized 

z_n_phen_rand_f <- read.csv("C:/Users/abomb/Box/HF and HS/R/HF HS/input/z_n_phen_rand_f.csv")

# anova

z_n_phen_rand_p <- read.csv("C:/Users/abomb/Box/HF and HS/R/HF HS/input/z_n_phen_rand_p.csv")

tr_z_n_phen_rand_g <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/tr_z_n_phen_rand_g.xlsx", 
                                 col_types = c("text", "text", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "text", "numeric"))

#glm
library(rio)
setwd("C:/Users/abomb/Box/HF and HS/R/HF HS/output")
convert("z_n_phen_rand_z.csv", "z_n_phen_rand_s.xlsx")

tr_z_n_phen_rand_p <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/tr_z_n_phen_rand_p.xlsx", 
                                 col_types = c("text", "text", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "text", "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "text", "numeric"))

tr_z_n_phen_rand_g <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/tr_z_n_phen_rand_g.xlsx", 
                                 col_types = c("text", "text", "text", 
                                               "text", "text", "text", "text", "text","text", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "text", "numeric"))
# alpha
transpose_100_zotu_unmod_norm <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/transpose 100 zotu unmod_norm.xlsx")

transpose_all_zotu_norm <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/transpose_all_zotu_norm.xlsx")
# anosim
hfhs_trans_zotu_normalized_sum_genera <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/hfhs_trans_zotu_normalized _sum_genera.xlsx")
# discirminant
hfhs_zn_alltax <- read_csv("C:/Users/abomb/Box/HF and HS/R/HF HS/input/hfhs_zn_alltax.txt", 
                           col_types = cols(`Genetic line` = col_character(), 
                                            Group = col_character(), Round = col_character()))
hfhs_zn10_s <- read_delim("C:/Users/abomb/Box/HF and HS/R/HF HS/input/hfhs_zn10_s.txt", 
                          "\t", escape_double = FALSE, col_types = cols(Genotype = col_character(), 
                                                                        Round = col_character()), trim_ws = TRUE)
# lm
hfhs_trans_zotu_normalized_sum_genera <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/hfhs_trans_zotu_normalized _sum_genera.xlsx")
# MDS
transpose_all_zotu_norm <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/transpose_all_zotu_norm.xlsx")

transpose_100_zotu_unmod_norm <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/transpose 100 zotu unmod_norm.xlsx")

hfhs_wunif_all <- read_excel("C:/Users/abomb/Box/HF and HS/R/HF HS/input/hfhs_wunif_all.xlsx")
# All diets

all_tax_zn_5gen <- read.delim("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_tax_zn_5gen.txt")

# wilcox
all_d_zn_f <- read.delim("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_zn_f.txt")
# anosim
all_d_zn_f <- read.delim("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_zn_f.txt")

all_d_zn10_f <- read.delim("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_zn10_f.txt")

# discirminant

all_d_zn10_g <- read_delim("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_zn10_g.txt", 
                           "\t", escape_double = FALSE, col_types = cols(Genotype = col_character(), 
                                                                         Round = col_character()), trim_ws = TRUE)

# updating file for wunif
all_d_wunif <- read_delim("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_wunif.txt", 
                          "\t", escape_double = FALSE, trim_ws = TRUE)

# MDS
all_d_zn_z <- read.delim("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_zn_z.txt")

all_d_wunif_5gen <- read_delim("C:/Users/abomb/Box/HF and HS/R/HF HS/input/all_d_wunif_5gen.txt", 
                               "\t", escape_double = FALSE, trim_ws = TRUE)

