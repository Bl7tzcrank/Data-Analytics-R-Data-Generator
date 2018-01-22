####R-Script for analysing the given dataset####

#Import libraries
install.packages("GGally")
install.packages("DMwR")
install.packages("ClassDiscovery")
install.packages("mvoutlier")
install.packages("proxy")
install.packages("dbscan")
install.packages("MVN")
install.packages("factoextra")
install.packages("rgl")
library(rgl)
library(factoextra)
library(MVN)
library(proxy)
library(mvoutlier)
library(GGally)
library(ggplot2)
library(DMwR)
library(dbscan)




#########functions##########

#1. Data Pre-processing#
#1.1 Exploratory Data Analysis#

#1.2 Data Transformation

#1.3 Outlier detection

#1.3.1 Identification of bivariate outliers using scatter plots

#Calculate standardized values; returns entry number
detectoutstand <- function(data){
  MeansCol <- apply(data,2, mean)
  CovMatrix = cov(data)
  SDCol = sqrt(diag(CovMatrix))
  temp = sweep(data,2,MeansCol)
  normalizedDataSet = apply(temp,1,function(x){
    x/SDCol
  })
  normalizedDataSet = t(matrix(normalizedDataSet,nrow = ncol(data),ncol = nrow(data)))
  #colnames(normalizedDataSet)<-colnames(data)
  allunivariateoutliersCalc = sort((which(abs(normalizedDataSet)>3.5)))
  return(allunivariateoutliersCalc)
}

#Calculate generalized squared distances and plots X2
detectoutx <- function(data){
  MeansCol <- apply(data,2, mean)
  CovMatrix = cov(data)
  d = apply(data, 1, function(x) t(x - MeansCol) %*% solve(CovMatrix) %*% (x-MeansCol))
  #qc = qchisq((seq_len(nrow(data)) - 1/2) / nrow(data), df = 9)
  #sort.d = sort(d)
  
  #1.3.5 Visualize the distances in a chi-square distribution
  #plot(qc, sort.d, las = 1, pch = 19, xlab = expression(paste(chi[4]^ 2,"-Quantile")), ylab = "Ordered Distances", xlim = range(qc) * c(0, 1.1), ylim = range(sort.d) * c(0, 1.1))
  #lines(qc,sort.d,col="green")
  #abline(h=23.59, col="red")
  return(d)
}

#1.4 Outlier removal#
#This function removes NA values from the dataset
naremoval <- function(data){
  for(i in 1:nrow(data)){
    if(any(is.na(data[i,]))){
      data = data[-i,]
    }
  }
  return(data)
}

#This functions identifies univariate outliers based on standardized values (>3.5) and multivariate outliers (based on Chi-square test)
#Univariate outliers are replaced by applying knn imputation 
#Observations with multivariate outliers are excluded from the data set
outlierremoval <-function(data){
  o <- detectoutstand(data)
  d <-as.matrix(data)
  
  for(i in 1:length(o)){
    d[[o[i]]] = NA
  }
  d <- as.data.frame(d)
  knnOutput <- knnImputation(d[, !names(d) %in% "medv"], meth = "median")  # perform knn imputation.
  z <- as.vector(which(detectoutx(knnOutput)>23.59))
  return(knnOutput[-z,])
}

#1.5 Testing for normal distribution#
#This function creates a matrix with QQ-Plots of a certain data frame (only numeric values allowed)
normaldistplots <- function(data){
  layout(matrix(1:15, ncol = 5, nrow = 3))
  sapply(colnames(data), function(x){
    xj = sort(data[[x]])
    rj = rank(data[[x]])
    js = (rj-0.5)/length(data[[x]])
    qj = qnorm(js)
    plot(qj,data[[x]],main = x, xlab = "Theoretical quantiles", ylab = "Obeserved quantiles")
    qqline(data[[x]], lwd = 2, col = "red")
  })
}

#This function creates a Chi-Square plot: a QQ-Plot for the X2-distribution 
#and can be used to test for multivariate normal distributions
mnormdistplots <- function(data){
  
  x = data
  cm = colMeans(x)
  S = cov(x)
  d = apply(x, 1, function(x){
    t(x - cm) %*% solve(S) %*% (x - cm)
  })
  sort.d = sort(d)
  
  qc = qchisq((seq_len(nrow(x)) - 1/2) / nrow(x), df = ncol(data))
  plot(qc, sort.d, las = 1, pch = 19,
       xlab = expression(paste(chi[7]^2, "-Quantile")),
       ylab = "Ordered Distances",
       xlim = range(qc) * c(0, 1.1),
       ylim = range(sort.d) * c(0, 1.1))
  
  rank.deviations = rank(abs(qc - sort.d), ties = "random")
  out = which(rank.deviations > nrow(x) - 3)
  text(qc[out], sort.d[out], names(out), pos = 2, col = "blue")
  abline(a = 0, b = 1, col = "red", lwd = 2)
}

#hist for normal distribution test
histo <-function(data){
  layout(matrix(1:15, ncol = 5, nrow = 3))
  sapply(colnames(data), function(x){
    hist(as.numeric(data[[x]]), main = x, xlab = "",
         col = "cyan", cex.main = 2)
  })
}

#Shapiro-Wilk test for not-normal distribution
hytest <-function(data){
  d <- scale(data)
  sapply(colnames(d), function(x){
    shapiro.test(as.numeric(d[,x]))[1:2]
  })
}

hytestexp <-function(data){
  d <- scale(data)
  sapply(colnames(d), function(x){
    ks.test(x = d, y = "pexp", rate = 1/mean(d))
  })
}

#2. Dimensionality Reduction#

#3. Cluster Analysis

#creates a dendrogram for the features f1, f2 of data with the given clustering method
agglomerative_dendrogram = function(data, f1, f2, method = "single"){
  x <- cbind(datasetadj[,f1],datasetadj[,f2])
  x <- scale(x)
  x <- as.data.frame(x)
  colnames(x) <- c(colnames(data)[f1], colnames(data)[f2])
  d <- dist(x, method="euclidean")
  h <- hclust(d, method=method)
  plot(h)
  return(h)
}

#creates the scree-plot of k = 1..6 and 10 replicates of the kmeans algorithm
#applied on the features 1 and 2 of the data-frame data.
kmeans_screeplot = function(data, f1, f2){
  x <- cbind(datasetadj[,f1],datasetadj[,f2])
  x <- scale(x)
  x <- as.data.frame(x)
  colnames(x) <- c(colnames(data)[f1], colnames(data)[f2])
  
  km <- lapply(1:6, function(k) replicate(10, kmeans(x, centers = k)$tot.withinss))
  wss_k = sapply(1:6, function(k) mean(km[[k]]))
  
  plot(x = 1:6, y = log10(wss_k),
       xlab = "k", ylab = expression("log(" * WSS[k] * ")"),
       las = 1, pch = 19, type = "o")
  return(x)
}

#Plots f1-f2 diagram with colours for calculated clusters
dbscan_plot = function(data, f1, f2, eps = .5, minpts = 10){
  x <- cbind(datasetadj[,f1],datasetadj[,f2])
  x <- scale(x)
  x <- as.data.frame(x)
  colnames(x) <- c(colnames(data)[f1], colnames(data)[f2])
  db <- dbscan(x, eps = eps, minPts = minpts)
  ggplot(x, aes(data[,f1], data[,f2], color = db$cluster)) + geom_point()
}

#function to remove observations with a certain value in a certain row
removeValue <- function(data,l,v){
  k = 1
  while(k <= nrow(data)){
    if(data[k,l] == v){
      data = data[-k,]
    }else{
      k = k+1
    }
  }
  return(data)
}

######### end of function declaration ############

######### main ##########
#Dataset description
#We created a dataset adopting the flirting-app “Tinder”, but for slightly older people, naming it “Finder”. Each user creates a profile and is able to change some settings and upload pictures. Our dataset includes some of those settings, the number of pictures uploaded as well as a “picky factor” used by the providers of the app. In general, swiping right means that the user wants to match with the shown user, swiping left is the opposite. We are only implementing a heterosexual version with two genders, meaning that women are matched to men only and other way round. For a more detailled description of the features please see the according subsection.
#We generate data for the following features:
#Age
#Gender (0=female, 1=male)
#Search Radius
#Picky-Factor
#Number of Characters in Bio per User
#Number of Pictures per User
#Age Search Range
#Matches

#Data loading
dataset = get(load("Datasets/data_01.rdata"))

#Exploratory data analysis
str(dataset)
summary(dataset)
#ggpairs(dataset[-c(1,3,11,15)])

#Initial NA replacement
datasetadj <- knnImputation(dataset[, !names(dataset) %in% "medv"], meth = "median")  # perform knn imputation.

#Outlier detection
pairs(datasetadj) #for detecting outliers visually
allBoxPlots = boxplot(log10(dataset)) #Visualization of outliers by using log10 boxplots
detectoutstand(datasetadj) #outlier detection outputting standardized values
c = detectoutx(datasetadj) #outlier detection outputting X2plot

#Outlier removal
datasetadj <- outlierremoval(datasetadj)

pairs(datasetadj)

#Plot dataset for analysis
ggpairs(datasetadj)

#Testing for normal distribution
normaldistplots(datasetadj) #visually by QQ-Plots
mnormdistplots(datasetadj) #visually by QQ-Plot
histo(datasetadj) #visually by histograms
checkNormSW = uniNorm(datasetadj, type="SW" , desc=TRUE) # Shapiro-Wilk's Normality Test
checkNormCVM = uniNorm(datasetadj, type="CVM" , desc=TRUE) # Cramer-von Mises's Normality test
checkNormLillie = uniNorm(datasetadj, type="Lillie" , desc=TRUE) # Lilliefors (Kolmogrov-Smirnov) Normality Test
checkNormSF = uniNorm(datasetadj, type="SF" , desc=TRUE) # Shapiro-Francia's Normality Test
checkNormAD = uniNorm(datasetadj, type="AD" , desc=TRUE) # Anderson-Darling's Normality Test

hytest(datasetadj) #Shapiro-Wilk test

MVN::hzTest(scale(datasetadj),qqplot = TRUE)

MVN::mardiaTest(datasetadj[],qqplot = TRUE)

MVN::mvnPlot(MVN::hzTest(datasetadj[,c(7,8)]))

MVN::uniPlot(datasetadj)

MVN::mvnPlot(datasetadj[,c(1,8)])

mnormdistplots(datasetadj)

#Should we check for the actual distribution
#install.packages("fitdistrplus")
#library(fitdistrplus)
#descdist(datasetadj)

##Clustering
#Only relations without gender (the results are obviously clustered after the two genders) and not
#between age_min or age_max because there are proportional to the age.
#We tried dbscan, agglomerative approaches(single-linkage, complete-linkage, centroid, average, ward) and
#k-means on the data. The most appropriate clustering was chosen and kept here.

#Kmeans on age and search_radius
f1 = 1
f2 = 3
x <- kmeans_screeplot(datasetadj, f1, f2)
k <- kmeans(x, 3) #clusters indicated by scree-plot
k$cluster <- as.factor(k$cluster)
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = k$cluster)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")

#Kmeans on age and pickyness
f1 = 1
f2 = 4
x <- kmeans_screeplot(datasetadj, f1, f2)
k <- kmeans(x, 3) #clusters indicated by scree-plot
k$cluster <- as.factor(k$cluster)
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = k$cluster)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")

#Agglomerative on age and #characters_bio
f1 = 1
f2 = 5
x <- as.data.frame(scale(cbind(datasetadj[,f1],datasetadj[,f2])))
h <- agglomerative_dendrogram(datasetadj, f1, f2, method = "complete")
c <- cutree(h, k = 2) #clusters indicated by dendrogram
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = c)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")

#Kmeans on age and #pictures
f1 = 1
f2 = 6
x <- kmeans_screeplot(datasetadj, f1, f2)
k <- kmeans(x, 3) #clusters indicated by scree-plot
k$cluster <- as.factor(k$cluster)
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = k$cluster)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")

#Kmeans on age and matches
f1 = 1
f2 = 9
x <- kmeans_screeplot(datasetadj, f1, f2)
k <- kmeans(x, 3) #clusters indicated by scree-plot
k$cluster <- as.factor(k$cluster)
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = k$cluster)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")

#Agglomerative on search radius and pickyness
f1 = 3
f2 = 4
x <- as.data.frame(scale(cbind(datasetadj[,f1],datasetadj[,f2])))
h <- agglomerative_dendrogram(datasetadj, f1, f2, method = "ward.D2")
c <- cutree(h, k = 3) #clusters indicated by dendrogram
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = c)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")

#Kmeans on search_radius and #characters_bio
f1 = 3
f2 = 5
x <- kmeans_screeplot(datasetadj, f1, f2)
k <- kmeans(x, 3) #clusters indicated by scree-plot
k$cluster <- as.factor(k$cluster)
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = k$cluster)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")

#Agglomerative on search_radius and #pictures
f1 = 3
f2 = 6
x <- as.data.frame(scale(cbind(datasetadj[,f1],datasetadj[,f2])))
h <- agglomerative_dendrogram(datasetadj, f1, f2, method = "complete")
c <- cutree(h, k = 2) #clusters indicated by dendrogram
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = c)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")

#Kmeans on search_radius and matches
f1 = 3
f2 = 9
x <- kmeans_screeplot(datasetadj, f1, f2)
k <- kmeans(x, 3) #clusters indicated by scree-plot
k$cluster <- as.factor(k$cluster)
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = k$cluster)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")

#Agglomerative on pickyness and #characters_bio
f1 = 4
f2 = 5
x <- as.data.frame(scale(cbind(datasetadj[,f1],datasetadj[,f2])))
h <- agglomerative_dendrogram(datasetadj, f1, f2, method = "complete")
c <- cutree(h, k = 2) #clusters indicated by dendrogram
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = c)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")

#Agglomerative on pickyness and #pictures
f1 = 4
f2 = 6
x <- as.data.frame(scale(cbind(datasetadj[,f1],datasetadj[,f2])))
h <- agglomerative_dendrogram(datasetadj, f1, f2, method = "complete")
c <- cutree(h, k = 2) #clusters indicated by dendrogram
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = c)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")

#Kmeans on pickyness and matches
f1 = 4
f2 = 9
x <- kmeans_screeplot(datasetadj, f1, f2)
k <- kmeans(x, 2) #clusters indicated by scree-plot
k$cluster <- as.factor(k$cluster)
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = k$cluster)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")


#Kmeans on #characters_bio and #pictures
f1 = 5
f2 = 6
x <- kmeans_screeplot(datasetadj, f1, f2)
k <- kmeans(x, 3) #clusters indicated by scree-plot
k$cluster <- as.factor(k$cluster)
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = k$cluster)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")

#Agglomerative on #characters_bio and matches
f1 = 5
f2 = 9
x <- as.data.frame(scale(cbind(datasetadj[,f1],datasetadj[,f2])))
h <- agglomerative_dendrogram(datasetadj, f1, f2, method = "ward.D2")
c <- cutree(h, k = 3) #clusters indicated by dendrogram
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = c)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")

#Kmeans on #pictures and matches
f1 = 6
f2 = 9
x <- kmeans_screeplot(datasetadj, f1, f2)
k <- kmeans(x, 2) #clusters indicated by scree-plot
k$cluster <- as.factor(k$cluster)
ggplot(x, aes(datasetadj[,f1], datasetadj[,f2], color = k$cluster)) + geom_point() + labs(x = colnames(datasetadj)[f1], y = colnames(datasetadj)[f2], color = "cluster")

##End of clustering

pairs(datasetadj, col=ifelse(datasetadj$gender==0, "red", "blue"))

#PCA
#Exclude supplementary qualitative variable from the PCA (naming Gender and Search Radius)
pr.comp = prcomp(datasetadj[,-c(2,3)], center = TRUE, scale=TRUE)#singular value decomposition
#pr.incomp = princomp(datasetadj[,-c(2,3)], cor=TRUE, scores=TRUE) #spectral decomposition approach

#Scree plot
eig.val <- get_eigenvalue(pr.comp) # Numerical representation of eigenvalues of the PCA
fviz_eig(pr.comp, addlabels = TRUE) # Visual representation of eigenvalues of the PCA

#Supplementary qualitative variables are prepared to use them for grouping the data later on
groupsgender <- as.factor(datasetadj$gender)
groupsRange <- as.factor(round(datasetadj$search_radius,0))

#Visualisation

fviz_pca_var(pr.comp,
             axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pr.comp,
             axes = c(3,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pr.comp,
                axes = c(3,2),
                col.var = "black",
                col.ind = groupsgender,
                palette = c("#00AFBB",  "#FC4E07"),
                label = "var"
)

fviz_pca_biplot(pr.comp,
                axes = c(3,2),
                col.var = "black",
                col.ind = groupsRange,
                #palette = c("#00AFBB",  "#FC4E07"),
                label = "var"
)



z <- sapply(groupsgender,function(x){
  if (x == 1 ) {x=1}else{x=2}
})

#3D plot highlighting the search radius
plot3d(pr.comp$x[,1:3], col = groupsRange, size=4)
text3d(pr.comp$rotation[,1:3], texts=rownames(pr.comp$rotation), col="red")
coords <- NULL
for (i in 1:nrow(pr.comp$rotation)) {
  coords <- rbind(coords, rbind(c(0,0,0),pr.comp$rotation[i,1:3]))
}
lines3d(coords, col="red", lwd=4)

#3D plot highlighting the gender cluster
plot3d(pr.comp$x[,1:3], col = z, size=4)
text3d(pr.comp$rotation[,1:3], texts=rownames(pr.comp$rotation), col="red")
coords <- NULL
for (i in 1:nrow(pr.comp$rotation)) {
  coords <- rbind(coords, rbind(c(0,0,0),pr.comp$rotation[i,1:3]))
}
lines3d(coords, col="red", lwd=4)

