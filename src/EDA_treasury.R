library(tidyverse)
library(moments)

treasury = read.csv("data/treasury.dat")
attach(treasury)
head(treasury)
summary(treasury)
str(treasury)

apply(treasury, 2, skewness) # comprobamos skewness en todas las variables
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

