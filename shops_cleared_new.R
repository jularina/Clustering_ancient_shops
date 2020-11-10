install.packages("dplyr")

library(cluster)
library(Rtsne)
library(ggplot2)
library(dplyr)


#Maybe you'll have to reopen file with UTF-8 or CP1251 encoding to read russian letters

Sys.setlocale("LC_CTYPE", "russian")

#Preparing data
shops <- read.table("shops_cleared_new.csv", header=T, sep=",",dec=".", encoding='CP1251')
shops$neighborhood = as.factor(shops$neighborhood)
shops$city = as.factor(shops$city)
shops$is_on_the_road= as.factor(shops$is_on_the_road)
shops$is_with_the_well= as.factor(shops$is_with_the_well)
shops$is_with_additional_services= as.factor(shops$is_with_additional_services)
shops$location = as.factor(shops$location)
shops$clusters_14 = as.factor(shops$clusters_14)

hist(shops$age)
var(shops$age)
shops1 = shops[,-c(1,2,11)]

#Gower distance
gower_df <- daisy(shops1, metric = "gower", type = list(logratio = 8))

#Plotting clusters
set.seed(2020)
tsne_object <- Rtsne(gower_df, is_distance = TRUE)

tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(shops$clusters_14))


ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster))
