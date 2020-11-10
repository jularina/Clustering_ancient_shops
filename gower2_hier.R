install.packages("dplyr")

#Maybe you'll have to reopen file with UTF-8 or CP1251 encoding to read russian letters

library(cluster)
library(Rtsne)
library(ggplot2)
library(dplyr)

Sys.setlocale("LC_CTYPE", "russian")

#Preparing data
shops <- read.table("shops_clear.csv", header=T, sep=",",dec=".", encoding='CP1251')
shops$neighborhood = as.factor(shops$neighborhood)
shops$city = as.factor(shops$city)
shops$is_on_the_road= as.factor(shops$is_on_the_road)
shops$is_with_the_well= as.factor(shops$is_with_the_well)
shops$is_with_additional_services= as.factor(shops$is_with_additional_services)
shops$shop_type = as.factor(shops$shop_type)
shops$location = as.factor(shops$location)


hist(shops$age)
var(shops$age)
shops1 = shops[,-c(1,2)]

#Gower distance
gower_df <- daisy(shops1, metric = "gower", type = list(logratio = 8))

#Clustering histogrm
ShopsClusters = hclust(gower_df, method = "complete")
plot(ShopsClusters)
rect.hclust(ShopsClusters, k=14, border="blue")
cl3 = cutree(ShopsClusters, k=3)
cl10 = cutree(ShopsClusters, k=10)
cl14 = cutree(ShopsClusters, k=14)


#Plotting clusters
set.seed(2020)
tsne_object <- Rtsne(gower_df, is_distance = TRUE)

tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(cl14))


ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster))

#Adding clusters to dataframe
shops$clusters_3 = cl3
shops$clusters_10 = cl10
shops$clusters_14 = cl14

write.csv(shops, 'shops_clear_clustered_hierarchi.csv')
