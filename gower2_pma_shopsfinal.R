install.packages("dplyr")


#Maybe you'll have to reopen file with UTF-8 or CP1251 encoding to read russian letters


library(cluster)
library(Rtsne)
library(ggplot2)
library(dplyr)

Sys.setlocale("LC_CTYPE", "russian")

shops <- read.table("shops_final.csv", header=T, sep=",",dec=".", encoding='CP1251')

#Preparing data
shops$neighborhood = as.factor(shops$neighborhood)
shops$city = as.factor(shops$city)
shops$is_on_the_road= as.factor(shops$is_on_the_road)
shops$is_with_the_well= as.factor(shops$is_with_the_well)
shops$is_with_additional_services= as.factor(shops$is_with_additional_services)
shops$shop_type = as.factor(shops$shop_type)
shops$location = as.factor(shops$location)
shops$clusters_14 = as.factor(shops$clusters_14)

shops$Последователи.Апокалипсиса = as.factor(shops$Последователи.Апокалипсиса)
shops$Бомбисты = as.factor(shops$Бомбисты)
shops$Рейдеры = as.factor(shops$Рейдеры)
shops$Стервятники = as.factor(shops$Стервятники)
shops$Воины.полураспада = as.factor(shops$Воины.полураспада)
shops$avg_benzac_month = as.factor(shops$avg_benzac_month)
shops$various_goods_types = as.factor(shops$various_goods_types)

#For new clusters
shops1 = shops[,-c(1,2, 11)]


#Gower distance
gower_df <- daisy(shops1, metric = "gower", type = list(logratio = c(8, 15, 16, 18, 19)))


#Silhouette graphic for pma clasterization
silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:18){
  pam_clusters = pam(as.matrix(gower_df),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(1:18, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:18, silhouette)

#Clustering PMA statistics
pam_shops17 = pam(gower_df, diss = TRUE, k = 17)
pam_shops22 = pam(gower_df, diss = TRUE, k = 22)
pam_shops14 = pam(gower_df, diss = TRUE, k = 14)


#Plotting PMA clusters and chosing best ones
set.seed(2020)
tsne_object <- Rtsne(gower_df, is_distance = TRUE)

tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_shops17$clustering))


ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster), size=2)

shops$cluster_id = pam_shops17$clustering


#Adding clusters to dataframe
shops_final = shops[, c(2, 23)]

write.csv(shops, 'shops_final_new.csv')
write.csv(shops_final, 'ids_clusters.csv')
