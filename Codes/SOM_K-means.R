# Load package
require(kohonen)

#Describing the data
data<-read.csv(file.choose(),header = T)
summary(data)

# Scale and centre
dt <- scale(data[, 7:23],center=TRUE)

# Prepare SOM
set.seed(590507)
som1 <- som(dt,
            somgrid(6,6, "hexagonal"),
            rlen=450,
            keep.data=TRUE)

# Plot codes map
myPal1=colorRampPalette(c("black","orange","red","green"))

plot(som1,
     type="codes",
     palette.name = myPal1,
     main="Codes",
     shape="straight",
     border ="gray")

# Extract the codebooks from SOM
cds <- as.data.frame(som1$codes)

# Compute WSS for up to 6 clusters for codebook vectors

wss <- (nrow(cds)-1)*sum(apply(cds,2,var))
for (i in 2:6){
  wss[i] <- sum(kmeans(cds,centers=i)$withinss)
}

# Plot the scree plot
par(mar = c(8,5,8,2))
plot(1:6, 
     wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares", 
     main="Within cluster sum of squares (WCSS)",
     col="blue",
     lwd =2)

# Scree plot - 3 clusters look sensible choice
nCls =3
som1.km <- kmeans(cds, nCls, nstart = 20)


# Plot the SOM codes map with 3 clusters as background
MyPal3 <- c("grey80", 'aquamarine', 'burlywood1')
par(mar = c(0,5,0,2))

plot(som1, 
     type="codes", 
     palette.name= myPal1,
     bgcol = MyPal3[som1.km$cluster], 
     main = "k-mean cluster",
     shape="straight",
     border ="gray"
)

legend("right",
       x=7,
       y=4,
       cex=1.5,
       title="Cluster",
       legend = c(1:nCls),
       fill= MyPal3[c(1:nCls)]
)


# Get the SOM cell number number assoicated with each of the data
SOM.clss <- as.data.frame(som1$unit.classif)
names(SOM.clss) <- "Cell.Nmbr"
unique(SOM.clss)

# Get the k-means 3-class classification of the 36 SOM cells
kMns.clst <- as.data.frame(som1.km$cluster)
names(kMns.clst) <- "Clstr"

# Add a SOM cell reference for a lookup table
kMns.clst$Cell.Nmbr <- 1:nrow(kMns.clst)

# Use the lookup table to map the cluster number to each datum
dt.clst <- merge(SOM.clss,kMns.clst,by="Cell.Nmbr")


# Add the cluster column to the original data
data.clst <- cbind(data,dt.clst)
# saving the csv
#write.csv(data.clst,"C:\\Users\\mrunm\\Documents\\Baidu\\som_clusters.csv", row.names = FALSE)
# Compute means as a reality check
aggregate(data.clst[,7:23], 
          by=list(data.clst$Clstr),
          FUN=mean
)

library(factoextra)
# Clustering
datadf=data[, !(colnames(data) %in% c("origin_city","dest_city","dest_province","city_id_o","city_id_d"))]
v_predictors <- c("Week1","Week2","Week3","Week4","Week5","Week6","Week7","Week8","Week9","Week10","Week11","Week12","Week13","Week14","Week15","Week16","Week17")
# Compute k-means with k = 3
set.seed(123)
res.km <- kmeans(scale(datadf[,2:18]), 3, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluster

fviz_cluster(res.km, data = datadf[,2:18],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
library(ggpubr)
# Dimension reduction using PCA
res.pca <- prcomp(datadf[,2:18],  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)
# Add Species groups from the original data sett
ind.coord$origin_province <- datadf$origin_province
# Data inspection
head(ind.coord)
# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

library(dplyr)
library(ggplot2)
ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "origin_province", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)