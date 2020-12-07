library(tidyverse)
library(moments)
library(ggpubr)
library(GGally)
library(car)

# c("#177e89", "#084c61", "#db3a34", "#ffc857") fav color palette

treasury = read.csv("data/treasury.dat")
attach(treasury)
head(treasury)
summary(treasury)
str(treasury)

dim(treasury) # comprobamos dimensiones

# BOXPLOT DE TREASURY
gather_tr = treasury %>% gather() # primer boxplot exploratorio
ggplot(gather_tr, aes(x=key, y=value)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, size = 7, face ="bold"))


# vemos que las variables no funcionan en el mismo rango, vamos
# a normalizarlas
treasury_norm = as.data.frame(apply(treasury, 2, scale, center = TRUE, scale = TRUE))
gather_tr_norm = treasury_norm %>% gather()

apply(treasury_norm, 2, skewness) # comprobamos skewness en todas las variables
apply(treasury_norm, 2, t.test)

# BOXPLOT DE TREASURY NORMALIZADO
ggplot(gather_tr_norm, aes(x=key, y=value)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, size = 7, face ="bold"))

# PLOT DE GGALLY, MANTENER COMENTADO A MENOS QUE SEA NECESARIO
# TARDA MUCHO EN PROCESAR
# ggpairs(treasury_norm)


# DENSITY DE TODAS LAS VARIABLES
treasury_norm %>% gather %>% ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density(color = "#084c61", fill = "#177e89", alpha = 0.8) +
  labs(title="Map of the densities for the variables in Treasury")



# TODAS LAS VARIABLES AGAINST LA VARIABLE OBJETIVO
treasury %>% 
  gather(key="variable", value="value", -X1MonthCDRate) %>%
  ggplot(aes(x=value, y=X1MonthCDRate)) + 
  geom_point(color="#084c61", alpha=0.5) +
  facet_wrap(~variable, scale="free") + labs(x="Variables in Treasury")


  
# crear los qq plots de todas las variables
plot_list = list()
for (i in 1:length(names(treasury))){
  p = ggqqplot(treasury[,i])
  plot_list[[i]] = p
}
annotate_figure(
  ggarrange(plotlist = plot_list),
  top = text_grob("QQPlot para las variables del dataset Treasury", face="bold")
)

