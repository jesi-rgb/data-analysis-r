library(tidyverse)
library(moments)
library(ggpubr)


treasury = read.csv("data/treasury.dat")
attach(treasury)
head(treasury)
summary(treasury)
str(treasury)

apply(treasury, 2, skewness) # comprobamos skewness en todas las variables
apply(treasury, 2, shapiro.test)
dim(treasury) # comprobamos dimensiones

gather_tr = treasury %>% gather() # primer boxplot exploratorio
ggplot(gather_tr, aes(x=key, y=value)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, size = 7, face ="bold"))


# vemos que las variables no funcionan en el mismo rango, vamos
# a normalizarlas
treasury_norm = as.data.frame(apply(treasury, 2, scale, center = TRUE, scale = TRUE))
gather_tr_norm = treasury_norm %>% gather()

# el plot ahora sí nos muestra info más relevante
ggplot(gather_tr_norm, aes(x=key, y=value)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, size = 7, face ="bold"))


library(GGally)
ggpairs(treasury_norm)

# c("#177e89", "#084c61", "#db3a34", "#ffc857") color palette

treasury_norm %>% gather %>% ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density(color = "#084c61", fill = "#177e89", alpha = 0.8) +
  labs(title="Map of the densities for the variables in Treasury")
