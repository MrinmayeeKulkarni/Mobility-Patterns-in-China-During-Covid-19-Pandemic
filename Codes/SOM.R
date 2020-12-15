library(kohonen)
library(RColorBrewer)
library(rgdal)
library(magrittr)

#or define palettes using RColorBrewer:
display.brewer.all() 
cols <- brewer.pal(10, "Paired")

#Similarly, but a color function that will be called upon from inside other functions. Easy to change to rainbow, etc.
terraincolors <- function(n, alpha = 1) {
  terrain.colors(n, alpha=alpha)[n:1]
}

# Choose Data File
data<-read.csv(file.choose(),header = T)
str(data)

# Selecting the only numeric columns
v_numeric <-colnames(data)[sapply(X=data,FUN=class) %in% c("numeric","integer")]
v_numeric <- v_numeric[!v_numeric %in% c("city_id_o","city_id_d")]
# scaling data
data_model <- scale(data[,v_numeric])
data_train_matrix <- as.matrix((data_model))

set.seed(120)
# SOM modeling
som_mob <- som(X=data_train_matrix,grid=somgrid(6,6,"hexagonal", toroidal = TRUE),rlen=350)

#details of the result
num_insta<- table(som_mob$unit.classif)
num_insta
# it is good that there are no null nodes
# Get the SOM cell number number assoicated with each of the data
SOM.clss <- as.data.frame(som_mob$unit.classif)
names(SOM.clss) <- "Cell.Nmbr"
unique(SOM.clss)
#write.csv(SOM.clss,"C:\\Users\\mrunm\\Documents\\Baidu\\som_cell2019.csv", row.names = FALSE)

# Training Progress
par(mfrow=c(1,1)) #no of plots to combine
par(mar=c(5.1,4.1,4.1,2.1)) #par sets or adjusts plotting parameters. 
plot(som_mob, type="changes")
plot(som_mob, type="counts",main="Node counts",palette.name = viridis::viridis)
plot(x=som_mob,type='codes',palette.name = viridis::viridis,codeRendering = "segments", shape = "straight")
plot(x=som_mob,type='dist.neighbours',palette.name = viridis::viridis,codeRendering = "segments", shape = "straight",main='SOM Neighbour distances 2020 Data')
plot(x=som_mob,type='dist.neighbours',palette.name = grey.colors,codeRendering = "segments", shape = "straight",main='SOM Neighbour distances 2019 Data')
plot(x=som_mob,type='mapping',palette.name = viridis::viridis, shape = "straight",main='Mapping Plot 2020')
#col=brewer.pal(3,"Dark2")
# Plotting heat maps separately by variables: Un-scaled
var <- 1
var_unscaled <- aggregate(as.numeric(data[,var]), by=list(som_mob$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(som_mob, type = "property", property=var_unscaled, main=names(data)[var], shape = "straight", palette.name = viridis::viridis)

#The below function produces a menu for multiple heat maps, if a property is chosen.
#Adapted from Shane Lynn, 14-01-2014. http://www.shanelynn.ie/ and Love Börjeson http://rstudio-pubs-static.s3.amazonaws.com/437468_136a369149e24f24a4d0c152860ab4c3.html

plotHeatMap <- function(som_model, data, variable=0){    
  #On Mac, this will require working XQuartz installation. Whatever that is.
  
  require(dummies)
  require(kohonen)
  terraincolors
  
  interactive <- TRUE
  
  while (interactive == TRUE){
    
    if (variable == 0){
      #show interactive window.
      color_by_var <- select.list(names(data), multiple=FALSE,
                                  graphics=TRUE, 
                                  title="Choose variable to color map by.")
      #check for user finished.
      if (color_by_var == ""){ #if user presses Cancel - we quit function        
        return(TRUE)
      }
      interactive <- TRUE
      color_variable <- data.frame(data[, color_by_var])
      
    } else {
      color_variable <- data.frame(data[, variable])
      color_by_var <- names(data)[variable]
      interactive <- FALSE
    }
    
    #if the variable chosen is a string or factor - 
    #Get the levels and ask the user to choose which one they'd like.
    
    if (class(color_variable[,1]) %in% c("character", "factor", "logical")){
      #want to spread this out into dummy factors - but colour by one of those.
      temp_data <- dummy.data.frame(color_variable, sep="_")
      chosen_factor <- select.list(names(temp_data), 
                                   multiple=FALSE,
                                   graphics=TRUE, 
                                   title="Choose level of variable for colouring")
      color_variable <- temp_data[, chosen_factor]
      rm(temp_data, chosen_factor)
      color_by <- color_variable
    } else {      
      #impute the missing values with the mean.
      color_variable[is.na(color_variable[,1]),1] <- mean(color_variable[,1], na.rm=TRUE)
      #color_by <- capVector(color_variable[,1])
      #color_by <- scale(color_by)  
      color_by <- color_variable[,1]
    }
    unit_colors <- aggregate(color_by, by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)
    plot(som_model, type = "property", property=unit_colors[,2], main=color_by_var, palette.name=terraincolors, shape = "straight")    
  }
}

par(mfrow=c(3,3)) #Set the matrix to match the no properties to compare.

#Call the function 
plotHeatMap(som_mob, data, variable=0) #variable=0 to display the menu.



# Creating legend for Unique Provinces
# Color palette - one color for each attribute level

color <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
v_legend <- sample(color,(length(unique(data$origin_province))))
names(v_legend) <- sort(unique(data$origin_province))
v_color <- v_legend[data$origin_province]

# Plotting with the legend
par(mar = c(10,4,4,2),xpd=TRUE)
plot(x=som_mob,type="mapping",pch=15,col=v_color,bty='n',main="Mapping plot for Origin Provinces 2020")
par(mar = c(5,15,1,2),xpd=TRUE)
legend("bottom",legend=names(v_legend),col=v_legend,pch=15)

# Extracting the codebooks from SOM
cdbk <- as.data.frame(som_mob$codes)

# Compute WSS for up to 6 clusters for codebook vectors to get the optimum clusters

wss <- (nrow(cdbk)-1)*sum(apply(cdbk,2,var))
for (i in 2:6){
  wss[i] <- sum(kmeans(cdbk,centers=i)$withinss)
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
som_mob.km <- kmeans(cdbk, nCls, nstart = 20)


# Plot the SOM codes map with 3 clusters as background
# Plot codes map
somcol=colorRampPalette(c("black","orange","red","green"))
clusters3 <- c("darkgrey", 'navyblue', 'lightgreen')
par(mar = c(0,5,0,2))

plot(som_mob, 
     type="codes", 
     palette.name= somcol,
     bgcol = clusters3[som_mob.km$cluster], 
     main = "K-means Cluster 2020 Data",
     codeRendering = "segments",
     shape="straight",
     border ="gray"
)

legend("right",
       x=7,
       y=4,
       cex=1.5,
       title="Cluster",
       legend = c(1:nCls),
       fill= clusters3[c(1:nCls)]
)

plot(som_mob, 
     type="mapping", 
     palette.name= somcol,
     bgcol = clusters3[som_mob.km$cluster], 
     main = "Clusters SOM 2020",
     shape="straight",
     border ="gray",
     pchs=""
)

legend("right",
       x=7,
       y=4,
       cex=1.5,
       title="Cluster",
       legend = c(1:nCls),
       fill= clusters3[c(1:nCls)]
)
# Using heirarchical Clustering to save the codebook vectors
groups=3
som.km <- cutree(hclust(dist(som_mob$codes[[1]])),groups)
res <- data
res$Clust <- som.km[som_mob$unit.classif]
res$X <- som_mob$grid$pts[som_mob$unit.classif,"x"]
res$Y <- som_mob$grid$pts[som_mob$unit.classif,"y"]

# Indexing the cluster to the grid points with winning nodes
SOM.clss <- as.data.frame(som_mob$unit.classif)
names(SOM.clss) <- "Cell.Nmbr"
unique(SOM.clss)

final <- cbind(data,as.data.frame(som_mob$codes)[som_mob$unit.classif,])
#final <- cbind(data,as.data.frame(som_mob$codes)[SOM.clss])
#write.csv(res,"C:\\Users\\mrunm\\Documents\\Baidu\\som_final2019.csv", row.names = FALSE)
