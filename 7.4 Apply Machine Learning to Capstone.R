### Clustering Climate Stations by Latitude, Longitude, and Elevation

# install packages

install.packages(c("cluster", "NbClust"))
library("cluster")
library("NbClust")

# Create Station Cluster Data

Stations.cluster <- Stations[, c(4:6)]
Stations.cluster <- scale(Stations.cluster)

# How many clusters does this method suggest? It could be 3 or 4? 

wssplot <- function(data, nc = 8, seed = 1234){
  wss <- (nrow(data) - 1)*sum(apply(data, 2, var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")
}

wssplot(Stations.cluster)

k = 4
set.seed(1)
fit.km <- kmeans(Stations.cluster, centers = k, iter.max = 1000)

# What do these results mean? 

Stations.Results <- table(fit.km$cluster, Stations$Climate.Division)
View(Stations.Results)

# Why am I getting an error here? 

clusplot(Stations.Results, clus = Stations.Results[2, ])

# Using another method to verify number of clusters

set.seed(1234)
nc <- NbClust(Stations.cluster, min.nc = 2, max.nc = 8, method = "kmeans")
barplot(table(nc$Best.n[1, ]),
        xlab = "Number of Clusters", ylab = "Number of Criteria",
        main = "Number of Clusters Chosen")

### Predicting future winter tempuratures using Linear Regression

TMIN.linear <- subset(Min.Tempurature, select = c("Year", "TMIN"))

cor(TMIN.linear)

plot(TMIN.linear)

TMIN.model <- lm(TMIN ~ Station.Name + Year, data = Min.Tempurature)

summary(TMIN.model)

confint(TMIN.model)

plot(TMIN.model, which = c(1, 2))
