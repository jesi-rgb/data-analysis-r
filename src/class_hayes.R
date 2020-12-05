library(tidyverse)
library(ggpubr)
library(class)
library(caret)
library(MASS)

hayes = read.csv("data/hayes-roth.dat")

eval_knn = function(train_data, train_labels, test=NA, k_neighbors=seq(3, 41, by=2), verbose = 0){
  print("Starting evaluation of KNN")
  
  k_values = vector("numeric", length = length(k_neighbors))
  accuracies = vector("numeric", length = length(k_neighbors))
  
  if(is.na(test)){
    test = train_data
  }
  
  i = 1
  for(k in k_neighbors){
    if(verbose == 1){
      print(paste("Going for k", k))
    }
    
    predictions = knn(train_data, test, train_labels, k=k)
    pred_diag = data.frame(predictions, train_labels)
    colnames(pred_diag) = c("predictions", "labels")
    
    accuracy = (pred_diag %>% filter(predictions == labels) %>% count()) / dim(pred_diag[,0])
    
    accuracies[i] = accuracy
    k_values[i] = k
    i = i + 1
  }
  
  
  results = data.frame(cbind(k_values, accuracies))
  results = as.data.frame(lapply(results, unlist))
  colnames(results) = c("K", "Accuracy")
  
  
  print(
    ggplot(results, aes(x = K, y = Accuracy)) +
      geom_line(lwd = 1.5, color = "#177e89") +
      geom_point(color = "#084c61", lwd = 2)
  )
  
  return(results)
}

eval_knn(hayes, hayes$Class)

plot_list = list()
i = 1
for(k in seq(3, 25, by=2)){
  result = knn(hayes, hayes, hayes$Class, k=k)

  table = data.frame(confusionMatrix(result, factor(hayes$Class))$table)
  
  plotTable = table %>%
    mutate(goodbad = ifelse(table$Prediction == table$Reference, "Correcto", "Incorrecto")) %>%
    group_by(Reference) %>%
    mutate(prop = Freq/sum(Freq))
  
  
  p = ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
    scale_fill_manual(values = c(Correcto = "green", Incorrecto = "red")) +
    theme_bw() +
    xlim(rev(levels(table$Reference))) + 
    labs(title = paste("Confusion Matrix for K:", k))
  
  plot_list[[i]] = p
  i = i + 1
}

ggpubr::ggarrange(plotlist = plot_list)


#### LDA ####
hayesLDA = lda(Class ~ Age + MaritalStatus + EducationalLevel, data = hayes)

predLDA = predict(hayesLDA)

plotDataLDA = data.frame(Classes = factor(hayes[,"Class"], c(1, 2, 3), c("Class 1", "Class 2", "No class")),
                      LD1 = predLDA$x[,1], 
                      LD2 = predLDA$x[,2])

LDAplot = ggplot(plotDataLDA) + geom_point(aes(LD1, LD2, colour = Classes, shape = Classes), size = 10) +
  scale_color_manual(values=c("#084c61", "#db3a34", "#ffc857"))


#### QDA ####
hayesQDA = qda(Class ~ Age + MaritalStatus + EducationalLevel, data = hayes)

predQDA = predict(hayesQDA)

plotDataQDA = data.frame(Classes = factor(hayes[,"Class"], c(1, 2, 3), c("Class 1", "Class 2", "No class")),
                      QD1 = predQDA$posterior[,1], 
                      QD2 = predQDA$posterior[,2],
                      QD3 = predQDA$posterior[,3])

QDAplot = ggplot(plotDataQDA) + geom_point(aes(QD1, QD2, alpha = QD3, colour = Classes, shape = Classes), size = 6) +
  scale_color_manual(values=c("#084c61", "#db3a34", "#ffc857"))

ggarrange(
  annotate_figure(LDAplot, top = text_grob("Linear Discriminant for the HR dataset", face = "bold")),
  annotate_figure(QDAplot, top = text_grob("Quadrant Discriminant for the HR dataset", face = "bold"))
)







