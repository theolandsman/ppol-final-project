library(tidyverse)
library(readxl)
library(smacof)
library(ggplot2)
AlaskaCVR <- read_excel("~/Documents/AlaskaCVR.xlsx", na = "under")
wyoming <- read_csv("~/Documents/Wyoming data import.csv")

wyoming_l <- wyoming %>%
  select(BallotID, `1st` ,`2nd` ,`3rd` ,`4th`, `5th` ) %>%
  pivot_longer(-BallotID, names_to = 'rank', values_to = "candidate") %>%
  filter(!candidate %in% c('under','over')) %>%
  pivot_wider(names_from = candidate, values_from = rank, values_fn = first)

### Data Preprocessing

w_matrix <- wyoming[,11:18]
# Impute missing values
rec <- recipe( ~ ., data = w_matrix)
pca_rcv <- rec %>%
  step_bagimpute(all_numeric()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_pca(all_numeric(), num_comp = 3)
pca_estimates <- prep(pca_rcv, training = w_matrix)
pca_data <- bake(pca_estimates, w_matrix)


### Cluster Analysis

library(broom)
w_pca <- pca_data
# predict four clusters
w_kmeans <-kmeans(w_matrix,centers = 4,# number of clusters
                  nstart = 100)# number of random starts
w_pca$choice1 <- wyoming$`1st`
w_pca$choice2 <- wyoming$`2nd`
tidy(w_kmeans)%>%knitr::kable(digits = 2)

w_pca$cluster<-w_kmeans$cluster
w_pca%>%
  group_by(cluster, choice1) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  spread(cluster,count)


## It appears that Biden supports (1) are overwhelmingly in cluster 3, while Sanders supporters are largely in Cluster 2. Cluster 4 seems to be voters who liked both Warren but also Biden and Sanders. Cluster 1 looks to be dominated by Biden supporters who also liked some of the more eccentric also-rans like Bloomberg and Steyer. 

ggplot(data = w_pca) +
  geom_point(aes(x = PC1, y =PC2, color = factor(cluster)),alpha = .4) + 
  theme_minimal()


### With three clusters

w_pca <- pca_data
# predict four clusters
w_kmeans <-kmeans(pca_data,centers = 3,# number of clusters
                  nstart = 100)# number of random starts
w_pca$choice1 <- wyoming$`1st`
w_pca$choice2 <- wyoming$`2nd`
tidy(w_kmeans)%>%knitr::kable(digits = 2)

w_pca$cluster<-w_kmeans$cluster
w_pca%>%
  group_by(cluster, choice1) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  spread(cluster,count)
ggplot(data = w_pca) +
  geom_point(aes(x = PC1, y =PC2, color = factor(cluster)),alpha = .4) + 
  theme_minimal()

## With three clusters, Cluster 3 is the Biden cluster, Cluster 1 is a combination of Biden and Sanders voters, and Cluster 2 is a combination of Sanders and Warren Supporters. Plotting the data, we can see that Clusters 2 and 3 occupy the extremes of the PC1 dimension, while Cluster 1 is more moderate. 


## This takes way too long, maybe a project for AWS!
library(factoextra)
fviz_nbclust(pca_data, FUN = kmeans, method = "wss")
# fviz_nbclust(pca_data, FUN = kmeans, method = "gap_stat") This one crashes because it runs out of memory
fviz_nbclust(pca_data, FUN = kmeans, method = "silhouette")


## Optimal cluster pattern analysis does not seem to yield a conclusive result, but 2 seems to be the best in terms of optimizing between them. 

### With two clusters

w_pca <- pca_data
# predict four clusters
w_kmeans <-kmeans(pca_data,centers = 2,# number of clusters
                  nstart = 100)# number of random starts
w_pca$choice1 <- wyoming$`1st`
w_pca$choice2 <- wyoming$`2nd`
tidy(w_kmeans)%>%knitr::kable(digits = 2)

w_pca$cluster<-w_kmeans$cluster
w_pca%>%
  group_by(cluster, choice1) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  spread(cluster,count)
ggplot(data = w_pca) +
  geom_point(aes(x = PC1, y =PC2, color = factor(cluster)),alpha = .4) + 
  theme_minimal()
ggplot(data = w_pca) +
  geom_density(aes(x = PC1, fill = factor(cluster)),alpha = .4) + 
  theme_minimal()


## Cluster 2, Biden Coalition, Cluster 1, numerically much smaller Sanders-Warren coalition. 