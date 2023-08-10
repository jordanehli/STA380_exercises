library(tidyverse)
library(ggplot2)
library(dplyr)
library(flexclust)

wine_data <- read_csv('./data/wine.csv')
summary(wine_data)

View(wine_data)

ggplot(wine_data) +  geom_point(aes(x=fixed.acidity, y=volatile.acidity, color=color))
ggplot(wine_data) +  geom_point(aes(x=alcohol, y=quality, color=color))

t <- wine_data %>% select(-color)
cor_values <- cor(t)
palette = colorRampPalette(c("green","white","red")) (20)
heatmap(x = cor_values, col = palette, symm = TRUE)

X = scale(t, center=TRUE, scale=TRUE)
clust2 = kmeans(X, 2, nstart = 25)


# Total sulfur dioxide vs Chlorides
ggplot(wine_data) + geom_point(aes(x=total.sulfur.dioxide, y=chlorides, color=color))
qplot(total.sulfur.dioxide, chlorides, data=wine_data, color=factor(clust2$cluster))

# Fixed acidity vs Volatile acidity
ggplot(wine_data) +  geom_point(aes(x=fixed.acidity, y=volatile.acidity, color=color))
qplot(fixed.acidity, volatile.acidity, data=wine_data, color=factor(clust2$cluster))

# Fixed acidity vs Density
ggplot(wine_data) +  geom_point(aes(x=fixed.acidity, y=density, color=color))
qplot(fixed.acidity, density, data=wine_data, color=factor(clust2$cluster))

# Alcohol vs pH
ggplot(wine_data) +  geom_point(aes(x=alcohol, y=pH, color=color))
qplot(alcohol, pH, data=wine_data, color=factor(clust2$cluster))

# template
ggplot(wine_data) +  geom_point(aes(x=citric.acid, y=sulphates, color=color))
qplot(citric.acid, sulphates, data=wine_data, color=factor(clust2$cluster))
