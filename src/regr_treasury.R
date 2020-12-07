library(tidyverse)
library(moments)
library(ggpubr)
library(GGally)
library(car)

# c("#177e89", "#084c61", "#db3a34", "#ffc857") fav color palette

treasury = read.csv("data/treasury.dat")
attach(treasury)

