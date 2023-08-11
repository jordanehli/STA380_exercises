library(tidyverse)
library(ggplot2)

summary(wine)

#normalize the data
library(caret)
process <- preProcess(as.data.frame(wine), method=c("range"))
norm_scale <- predict(process, as.data.frame(wine))

#removing color since it is not numerical, then getting the means of all the variables
wine_results = norm_scale %>%
  select(-color)

wine_results

#Getting the first PC1
PCA = prcomp(wine_results, scale = TRUE)
plot(PCA)
summary(PCA)

PCA$rotation

wines = merge(wine, PCA$x[,1:3], by.x=1, by.y = 0)

# PC1 explains 25.35% of the variance in the model. 
ggplot(wines) + 
  geom_col(aes(x=reorder(color, PC1), y=PC1)) + 
  coord_flip()

# PC2 explains 22.08% of the variance in the model.
ggplot(wines) + 
  geom_col(aes(x=reorder(color, PC2), y=PC2)) + 
  coord_flip()

# PC3 explains 13.68% of the variance in the model. 
ggplot(wines) + 
  geom_col(aes(x=reorder(color, PC3), y=PC3)) + 
  coord_flip()
