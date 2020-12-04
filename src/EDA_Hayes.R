library(tidyverse)
library(ggpubr)
library(moments)
library(scatterpie)
library(ggridges)

hayes = read.csv("data/hayes-roth.dat")

str(hayes)

ggplot(data = hayes) +
  geom_bar(aes(x = Age), color = "yellow", fill = "yellow", alpha = 0.5) +
  geom_bar(aes(x = Hobby), color = "red", fill = "red", alpha = 0.5) +
  geom_bar(aes(x = MaritalStatus), color = "green", fill = "green", alpha = 0.5) +
  geom_bar(aes(x = EducationalLevel), color = "blue", fill = "blue", alpha = 0.2) +
  theme(legend.position = "top")

p_age = ggplot(data = hayes) + geom_bar(aes(x = Age), color = "yellow", fill = "yellow", alpha = 0.5)
p_hobby = ggplot(data = hayes) + geom_bar(aes(x = Hobby), color = "red", fill = "red", alpha = 0.5)
p_mari = ggplot(data = hayes) + geom_bar(aes(x = MaritalStatus), color = "green", fill = "green", alpha = 0.5)
p_edu = ggplot(data = hayes) + geom_bar(aes(x = EducationalLevel), color = "blue", fill = "blue", alpha = 0.5)

p_arr = ggarrange(plotlist = list(p_age, p_hobby, p_mari, p_edu))
annotate_figure(p_arr, top = text_grob("Bar graph for the Hayes-Roth dataset", face = "bold"))

d_age = ggplot(data = hayes) +   geom_density(aes(x = Age), color = "yellow", fill = "yellow", alpha = 0.5)
d_hobby = ggplot(data = hayes) + geom_density(aes(x = Hobby), color = "red", fill = "red", alpha = 0.5)
d_mari = ggplot(data = hayes) +  geom_density(aes(x = MaritalStatus), color = "green", fill = "green", alpha = 0.5)
d_edu = ggplot(data = hayes) +   geom_density(aes(x = EducationalLevel), color = "blue", fill = "blue", alpha = 0.5)

d_arr = ggarrange(plotlist = list(d_age, d_hobby, d_mari, d_edu))
annotate_figure(d_arr, top = text_grob("Density plot for the Hayes-Roth dataset", face = "bold"))

# skewness
skewness(hayes$Hobby)
skewness(hayes$MaritalStatus)
skewness(hayes$Age)
skewness(hayes$EducationalLevel)


# check for normal dist (points should lie within the grey area)
q_hobby = ggqqplot(hayes$Hobby)
q_mari = ggqqplot(hayes$MaritalStatus)
q_age = ggqqplot(hayes$Age)
q_edu = ggqqplot(hayes$EducationalLevel)

q_arr = ggarrange(plotlist = list(q_age, q_hobby, q_mari, q_edu)) + labs(title = "Q-Q plot for the Hayes-Roth dataset")
annotate_figure(q_arr, top = text_grob("Q-Q plot for the Hayes-Roth dataset", face = "bold"))

shapiro.test(hayes$Hobby)

f_hayes = hayes
f_hayes$Hobby = factor(f_hayes$Hobby, c(1, 2, 3), c("Sport", "Chess", "Stamps"))
f_hayes$MaritalStatus = factor(f_hayes$MaritalStatus, c(1, 2, 3, 4), c("Single", "Married", "Divorced", "Widowed"))
f_hayes$Age = factor(f_hayes$Age, c(1, 2, 3, 4), c("30's", "40's", "50's", "60's"))
f_hayes$EducationalLevel = factor(f_hayes$EducationalLevel, c(1, 2, 3, 4), c("Junior High", "High School", "Trade School", "College"))
f_hayes$Class = factor(f_hayes$Class, c(1, 2, 3), c("Class 1", "Class 2", "No Class"))

annotate_figure(
  ggplot(data = f_hayes) + 
    geom_count(aes(x = Age, y = MaritalStatus, color = sort(EducationalLevel))) +
    scale_color_manual(values=c("#177e89", "#084c61", "#db3a34", "#ffc857")) +
    scale_size(range = c(0, 50)),
  top = text_grob("Point map for the main variables in the HR dataset", face = "bold")
)


p_age = ggplot(data = f_hayes) + geom_bar(aes(x = Age), color = "#177e89", fill = "#177e89", alpha = 0.8)
p_hobby = ggplot(data = f_hayes) + geom_bar(aes(x = Hobby), color = "#084c61", fill = "#084c61", alpha = 0.8)
p_mari = ggplot(data = f_hayes) + geom_bar(aes(x = MaritalStatus), color = "#db3a34", fill = "#db3a34", alpha = 0.8)
p_edu = ggplot(data = f_hayes) + geom_bar(aes(x = EducationalLevel), color = "#ffc857", fill = "#ffc857", alpha = 0.8)

p_arr = ggarrange(plotlist = list(p_age, p_hobby, p_mari, p_edu))
annotate_figure(p_arr, top = text_grob("Bar graph for the Hayes-Roth dataset", face = "bold"))


percent_graph_bar <- function(dataset, x_value, fill){
  plotdata = dataset %>%
    group_by({{x_value}}, {{fill}}) %>%
    summarize(n = n()) %>% 
    mutate(Percentage = n/sum(n),
           lbl = scales::percent(Percentage))
  
  print(
  ggplot(plotdata, aes(x = {{x_value}}, y = Percentage, fill = {{fill}})) +
    geom_bar(position = "fill", stat="identity") +
    scale_fill_manual(values=c("#177e89", "#084c61", "#db3a34", "#ffc857")) +
    geom_text(aes(label = lbl), 
              size = 3, 
              position = position_stack(vjust = 0.5), fontface="bold", color="white")
  )
}

percent_graph_bar(dataset = f_hayes, x_value=Age, fill=MaritalStatus)
percent_graph_bar(dataset = f_hayes, x_value=Age, fill=EducationalLevel)
percent_graph_bar(dataset = f_hayes, x_value=MaritalStatus, fill=EducationalLevel)

annotate_figure(
  ggplot(data = f_hayes) + 
    geom_count(aes(x = EducationalLevel, y = MaritalStatus, color = sort(Class), alpha = 1)) +
    scale_color_manual(values=c("#084c61", "#db3a34", "#ffc857")) +
    scale_size(range = c(0, 50)),
  top = text_grob("Point map for the main variables and the output variable in the HR dataset", face = "bold")
)


