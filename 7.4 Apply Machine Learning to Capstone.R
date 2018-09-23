### Clustering Climate Stations by Latitude, Longitude, and Elevation

### Is there a way to preference one or two data features over others when clustering? 

# Install packages

install.packages(c("cluster", "NbClust"))
library("cluster")
library("NbClust")
library("dplyr")
library("tidyr")
library("ggmap")

# Scale data for clustering

Stations.cluster <- Stations[, c(1, 4:6)]
Stations.cluster[c(2:4)] <- lapply(Stations.cluster[2:4], function(x) c(scale(x)))

# Determine number of clusters: Method 1

wssplot <- function(data, nc = 8, seed = 1234){
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")
}

wssplot(Stations.cluster[c(2:4)])

# Determine number of clusters: Method 2

set.seed(1234)
nc <- NbClust(Stations.cluster[2:4], min.nc = 2, max.nc = 8, method = "kmeans")
barplot(table(nc$Best.n[1, ]),
        xlab = "Number of Clusters", ylab = "Number of Criteria",
        main = "Number of Clusters Chosen")

# Apply k-means clustering

k = 4
set.seed(1234)
fit.km <- kmeans(Stations.cluster[c(2:4)], centers = k, iter.max = 1000)

# Add clusters to Stations dataset

Stations$Cluster <- as.factor(fit.km$cluster)
View(Stations)

# View clusters on a map

MN_Map <- get_map("Minnesota", zoom = 6)

ggmap(MN_Map) + 
  geom_point(aes(x = Longitude, y = Latitude, color = Cluster), data = Stations)

### Predicting lyme disease infection

lyme_infections <- read.csv("ld-Case-Counts-by-County-00-16.csv")

lyme_infections <- subset(lyme_infections, lyme_infections$Stname == "Minnesota")

lyme_infections$STCODE <- NULL

lyme_infections$CTYCODE <- NULL

lyme_infections$Stname <- NULL

names(lyme_infections) = gsub(pattern = "Cases", replacement = "", x = names(lyme_infections))

lyme_infections$Ctyname = gsub(" County", "", lyme_infections$Ctyname)

lyme_infections <- lyme_infections %>% 
  gather(Year, Infections, 2:18)

lyme_infections <- lyme_infections %>% 
  mutate_at(vars(Ctyname), .funs = toupper)

colnames(lyme_infections)[1] <- "County"

Max.Stations <- merge(Max.Tempurature, Stations, by = "Station.Name")

Max.Stations[, 12:14] <- NULL

Max.Stations <- merge(Max.Stations, lyme_infections, by = c("County", "Year"))

Infections.linear <- subset(Max.Stations, select = 
                        c("TMAX", "Year", "Month", "Cluster", "Climate.Division", 
                          "Latitude", "Longitude", "Elevation", "Infections"))

Infections.linear[1:9] <- sapply(Infections.linear[1:9], as.numeric)

cor(TMIN.linear)

Infections.model <- lm(Infections ~ TMAX + Year + Month + Latitude + Longitude + 
                   Elevation + Cluster, data = Infections.linear)

summary(Infections.model)

confint(Infections.model)

plot(Infections.model, which = c(1, 2))
