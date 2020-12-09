library(tidyverse)
library(moments)
library(ggpubr)
library(GGally)
library(car)

# c("#177e89", "#084c61", "#db3a34", "#ffc857") fav color palette

treasury = read.csv("data/treasury.dat")
attach(treasury)

#### REGRESIÓN LINEAL SIMPLE ####

# vamos a probar modelos simples con moneyStock, X3MRateSecondaryMarket, 
# X3YCMaturityRate y X30YCMortgageRate

#### against moneyStock ####
lm_money = lm(X1MonthCDRate~moneyStock, data=treasury)
summary(lm_money)

ggplot(treasury, aes(y = X1MonthCDRate, x = moneyStock)) +
  geom_abline(intercept = lm_money$coefficients[[1]], 
              slope = lm_money$coefficients[[2]], 
              color = "#ffc857", lwd=1.35, alpha=0.9) +
  geom_point(color = "#084c61") +
  geom_point(aes(fitted(lm_money)), color = "#db3a34", alpha = 0.5) 

confint(lm_money)

#### against X3MRateSecondaryMarket ####
lm_second_market = lm(X1MonthCDRate~X3M.Rate.SecondaryMarket, data=treasury)
summary(lm_second_market)

ggplot(treasury, aes(y = X1MonthCDRate, x = X3M.Rate.SecondaryMarket)) +
  geom_abline(intercept = lm_second_market$coefficients[[1]], 
              slope = lm_second_market$coefficients[[2]], 
              color = "#ffc857", lwd=1.35, alpha=0.7) +
  geom_point(color = "#084c61") + 
  geom_point(aes(fitted(lm_second_market)), color = "#db3a34", alpha = 0.5)

confint(lm_second_market)

#### against X3YCMaturityRate ####
lm_maturity = lm(X1MonthCDRate~X3Y.CMaturityRate, data=treasury)
summary(lm_maturity)

ggplot(treasury, aes(y = X1MonthCDRate, x = X3Y.CMaturityRate)) +
  geom_abline(intercept = lm_maturity$coefficients[[1]], 
              slope = lm_maturity$coefficients[[2]], 
              color = "#ffc857", lwd=1.35, alpha=0.7) +
  geom_point(color = "#084c61") + 
  geom_point(aes(fitted(lm_maturity)), color = "#db3a34", alpha = 0.5)

confint(lm_maturity)


#### against X30YCMortgageRate ####
lm_mortgage = lm(X1MonthCDRate~X30Y.CMortgageRate, data=treasury)
summary(lm_mortgage)

ggplot(treasury, aes(y = X1MonthCDRate, x = X30Y.CMortgageRate)) +
  geom_abline(intercept = lm_mortgage$coefficients[[1]], 
              slope = lm_mortgage$coefficients[[2]], 
              color = "#ffc857", lwd=1.35, alpha=0.7) +
  geom_point(color = "#084c61") + 
  geom_point(aes(fitted(lm_mortgage)), color = "#db3a34", alpha = 0.5)
  
confint(lm_mortgage)




#### REGRESIÓN MÚLTIPLE #####

#### lm all ####
lm_all = lm(X1MonthCDRate~., data=treasury)
sm_all = summary(lm_all)

#### lm hypothesis ####
lm_hypothesis = lm(X1MonthCDRate~X1Y.CMaturityRate+X3Y.CMaturityRate+
                     currency + moneyStock + tradeCurrencies, data=treasury)
sm_hypothesis = summary(lm_hypothesis)

#### lm best ####
lm_best = lm(X1MonthCDRate~moneyStock+X30Y.CMortgageRate+ 
             X3Y.CMaturityRate+X3M.Rate.SecondaryMarket)
sm_best = summary(lm_best)

#### lm money maturity
lm_money_maturity = lm(X1MonthCDRate~moneyStock+X3Y.CMaturityRate)
sm_money_maturity = summary(lm_money_maturity)


#### crear dataframe para ver las diferencias ####
sm_data = data.frame(RSquared = c(sm_all$r.squared,
                                  sm_hypothesis$r.squared,
                                  sm_best$r.squared,
                                  sm_money_maturity$r.squared),
                     AdjRSquared = c(sm_all$adj.r.squared,
                                     sm_hypothesis$adj.r.squared,
                                     sm_best$adj.r.squared,
                                     sm_money_maturity$adj.r.squared),
                     Model = c('All', 'Hypothetic', 'Best', 'Money+Maturity'))

sm_data %>% gather(key="variable", value="Accuracy", -Model) %>%
        ggplot(aes(fill=variable, y=Accuracy, x=Model)) + 
            geom_bar(position="dodge", stat="identity") +
            scale_fill_manual(values=c("#084c61", "#ffc857")) +
            coord_cartesian(ylim=c(0.99, 0.996))


#### Modelos no lineales e interacciones ####

nlm_best = lm(X1MonthCDRate ~ moneyStock * X3Y.CMaturityRate * X3M.Rate.SecondaryMarket)
summary(nlm_best)
ggplot(treasury, aes(x=moneyStock * X3Y.CMaturityRate  * X3M.Rate.SecondaryMarket, y = X1MonthCDRate)) +
  geom_point(color = "#084c61") +
  geom_point(aes(fitted(nlm_best)), color="#ffc857")


complex_formula = X30Y.CMortgageRate + moneyStock + X3Y.CMaturityRate + 
  X3M.Rate.AuctionAverage +
  I(1/loansLeases) * I(1/savingsDeposits) *
  I(1/currency) * I(1/federalFunds)

complex_model = lm(X1MonthCDRate ~ complex_formula)

ggplot(treasury, aes(x=complex_formula, y = X1MonthCDRate)) +
  geom_point(color = "#084c61") +
  geom_point(aes(fitted(complex_model)), color="#ffc857") +
  xlab("Comnbinación de todas las variables en complex_model")

summary(complex_model)





