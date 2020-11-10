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
summary(gower_df)

#Silhouette graphic for pma clasterization to understand which number of clusters is better to use
silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:15){
  pam_clusters = pam(as.matrix(gower_df),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(1:15, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:15, silhouette)

#Clustering PMA statistics - will choose 14 because of silhouette width
pam_shops11 = pam(gower_df, diss = TRUE, k = 11)
pam_shops3 = pam(gower_df, diss = TRUE, k = 3)
pam_shops14 = pam(gower_df, diss = TRUE, k = 14)


#Plotting clusters
set.seed(2020)
tsne_object <- Rtsne(gower_df, is_distance = TRUE)

tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_shops14$clustering))


ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster))


#Adding clusters to dataframe
shops$clusters_3 = pam_shops3$clustering
shops$clusters_14 = pam_shops14$clustering
shops$clusters_11 = pam_shops11$clustering

write.csv(shops, 'shops_clear_clustered_pma.csv')
