library(tidyverse)
library(moments)
library(ggpubr)
library(GGally)
library(car)

# c("#177e89", "#084c61", "#db3a34", "#ffc857") fav color palette

treasury = read.csv("data/treasury.dat")
attach(treasury)

# REGRESIÓN LINEAL SIMPLE
# vamos a probar modelos simples con moneyStock, X3MRateSecondaryMarket, 
# X3YCMaturityRate y X30YCMortgageRate

# against moneyStock
lm_money = lm(X1MonthCDRate~moneyStock, data=treasury)
summary(lm_money)

ggplot(treasury, aes(y = X1MonthCDRate, x = moneyStock)) +
  geom_point(color = "#084c61") + 
  geom_abline(intercept = lm_money$coefficients[[1]], 
              slope = lm_money$coefficients[[2]], 
              color = "#ffc857", lwd=1.35, alpha=0.7)

confint(lm_money)

# against X3MRateSecondaryMarket
lm_second_market = lm(X1MonthCDRate~X3M.Rate.SecondaryMarket, data=treasury)
summary(lm_second_market)

ggplot(treasury, aes(y = X1MonthCDRate, x = X3M.Rate.SecondaryMarket)) +
  geom_point(color = "#084c61") + 
  geom_abline(intercept = lm_second_market$coefficients[[1]], 
              slope = lm_second_market$coefficients[[2]], 
              color = "#ffc857", lwd=1.35, alpha=0.7)

confint(lm_second_market)

# against X3YCMaturityRate
lm_maturity = lm(X1MonthCDRate~X3Y.CMaturityRate, data=treasury)
summary(lm_maturity)

ggplot(treasury, aes(y = X1MonthCDRate, x = X3Y.CMaturityRate)) +
  geom_point(color = "#084c61") + 
  geom_abline(intercept = lm_maturity$coefficients[[1]], 
              slope = lm_maturity$coefficients[[2]], 
              color = "#ffc857", lwd=1.35, alpha=0.7)

confint(lm_maturity)
View(treasury)

# against X30YCMortgageRate
lm_mortgage = lm(X1MonthCDRate~X30Y.CMortgageRate, data=treasury)
summary(lm_mortgage)

ggplot(treasury, aes(y = X1MonthCDRate, x = X30Y.CMortgageRate)) +
  geom_point(color = "#084c61") + 
  geom_abline(intercept = lm_mortgage$coefficients[[1]], 
              slope = lm_mortgage$coefficients[[2]], 
              color = "#ffc857", lwd=1.35, alpha=0.7)

confint(lm_mortgage)

