library(ggplot2)
p1 <- ggplot(iris, aes(Petal.Length, Petal.Width, color= Species))+geom_point()
p4 <- ggplot(iris, aes(Sepal.Length , Sepal.Width, color=Species))+geom_point()

# Petal.Length and Petal.Width were similar among the same species but varied 
# considerably between different species.

#Clustering algorithm in R is K-Means which Performs k-means clustering on a data matrix. 

set.seed(20)
iriscluster <-  kmeans(iris[,1:2], 3, nstart = 9)
table(iriscluster$cluster , iris$Species) #table of clusters

library(dplyr)
iris %in% ggplot(aes(sepal.Length , Sepal.Width ,
                     color=factor(iriscluster$cluster)))+geom_point()



# The list iriscluster contains seven interesting elements:
# iriscluster$cluster: Indicates the cluster of each observation
# iriscluster$centers: The cluster centres
# iriscluster$totss: The total sum of squares
# iriscluster$withinss: Within sum of square. 
# The number of components return is equal to `k`
# iriscluster$tot.withinss: Sum of withinss
# irisclusterbetweenss: Total sum of square minus Within sum of square
# iriscluster$size: Number of observation within each cluster




 iris %>% ggplot(aes(Petal.Length, Petal.Width))+
   geom_point(aes(color=factor(iriscluster$cluster)))->p2

install.packages("animation")
library(animation)
kmeans.ani(iris[,3:4], 3) #for Petal Length vs Petal Width

kmeans_withiness <-  function(k){
  cluster <- kmeans(iris[,3:4], k)
  return (cluster$tot.withinss)
  
}

# Code Explanation
# function(k): Set the number of arguments in the function
# kmeans(rescale_df, k): Run the algorithm k times
# return(cluster$tot.withinss): Store the total within clusters sum of squares


#Set the Max Cluster
max_k <- 50
wss <- sapply(2:max_k, kmeans_withiness)


# Code Explanation
# max_k <-20: Set a maximum number of to 20
# sapply(2:max_k, kmean_withinss): 
# Run the function kmean_withinss() over a range 2:max_k, i.e. 2 to 20.


# Create a data frame to plot the graph
elbow <- data.frame(2:max_k , wss)
#Plot the graph with ggplot
ggplot(elbow , aes(x= X2.max_k , y= wss)) + geom_point() +
      geom_line() + scale_x_continuous(breaks = seq(1,20 , by = 1))





