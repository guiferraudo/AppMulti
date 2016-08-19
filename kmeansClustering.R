###Start function here
kmeansf = function(dataset, nclusters){
  set.seed(1950)
kmeansFit = kmeans(dataset, centers = nclusters, iter.max = 20, algorithm = "Hartigan-Wong", nstart = 1)  
#print 1: numberCasesByCluster 
numberCasesByCluster = table(kmeansFit$cluster)
# adding a new column into dataset to receive the cluster labels 
dfImportFinal = data.frame(dataset,cluster = as.factor(kmeansFit$cluster))
# redefining dataset to facilitate the calculus of average and standar deviation by cluster
dfImportProfileplot = reshape(dfImportFinal, direction = "long", 
                                    varying = list(1:ncol(dataset)),v.names="valores",
                                    times = names(dfImportFinal[,1:ncol(dataset)]),
                                    timevar = "y",
                                    ids = row.names(dfImportFinal))
#redefining the order of variable to avoid alphabetic order
dfImportProfileplot$y = as.factor(dfImportProfileplot$y)
levels(dfImportProfileplot$y) = colnames(dataset)

#Print 2: avg and sd
# Descriptives statistics by cluster
# average
avgEachCluster = tapply(dfImportProfileplot$valores,
                        list(dfImportProfileplot$cluster,dfImportProfileplot$y),mean)
# standard-deviation
sdEachCluster = tapply(dfImportProfileplot$valores,
                       list(dfImportProfileplot$cluster,dfImportProfileplot$y),sd)
summaryByCluster = list(avg=avgEachCluster,sd = sdEachCluster)
# get the names of members by cluster
membersEachCluster = list()
for(i in 1 :nlevels(dfImportFinal$cluster)) {membersEachCluster[[i]] =
  rownames(dfImportFinal[dfImportFinal$cluster == i,])}
names(membersEachCluster) = levels(dfImportFinal$cluster)

# printing the centroid (average value) by cluster, original scale, found by k-means
centroidsKmeans = avgEachCluster
# estimating euclidean distance among centroids by cluster divided by number of clusters
numberOfClusters = nrow(kmeansFit$centers)
dfCentroidsKmeansMatrixDist = dist(kmeansFit$centers, method="euclidean")/numberOfClusters

# Cluster member and its distances 
# list of data storage by cluster
db = list()
# list of centroid storage by cluster
centroid = list()
# list of euclidian distance storage by cluster 
distCentroid = list()

for(i in 1:numberOfClusters){
  
  db[[i]] = dfImportFinal[dfImportFinal$"cluster" == i,-ncol(dfImportFinal)]
  centroid[[i]] = (as.matrix(kmeansFit$centers[i,]))
  # distância euclidiana entre cada elemento do grupo e o respectivo centróide
  distCentroid[[i]] = apply(db[[i]],1,
                                     function(y){sqrt(sum((y-centroid[[i]])^2))})/numberOfClusters
}

names(db) = paste("cluster", 1:numberOfClusters, sep="")
names(centroid) = paste("cluster", 1:numberOfClusters, sep="")
names(distCentroid) = paste("cluster", 1:numberOfClusters, sep="")

return(list(originalDataByCluster = db, centroids = centroid, distToCentroid = distCentroid,
            avgByCluster = avgEachCluster, sdByCluster = sdEachCluster,
            dataToPlot = dfImportProfileplot
      )    )

}

