####R-Script for analysing the given dataset####

#Import libraries
install.packages("GGally")
install.packages("DMwR")
install.packages("ClassDiscovery")
install.packages("mvoutlier")
install.packages("proxy")
install.packages("dbscan")
install.packages("MVN")
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

#3. Cluster Analysis#
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

#Agglomerative on age and pickyness by gender [Better than k-Means!]
ggplot(datasetadj, aes(datasetadj[,1], datasetadj[,4], color = datasetadj[,2])) + geom_point()
x <- cbind(datasetadj[,1],datasetadj[,4], datasetadj[,2])
x <- scale(x)
x <- as.data.frame(x)
colnames(x) <- c("age", "pickyness", "gender")
d <- dist(x, method="euclidean")
h <- hclust(d, method="single")
plot(h) #2 clusters seems to be appropriate
cluster <- cutree(h, k = 2)
table(cluster,x[,3])
ggplot(x, aes(age, pickyness, color = cluster)) + 
  geom_point()

#Agglomerative on #characters_bio and matches by gender [Better than k-Means!]
ggplot(datasetadj, aes(datasetadj[,5], datasetadj[,9], color = datasetadj[,2])) + geom_point()
x <- cbind(datasetadj[,5],datasetadj[,9], datasetadj[,2])
x <- scale(x)
x <- as.data.frame(x)
colnames(x) <- c("char", "matches", "gender")
d <- dist(x, method="euclidean")
h <- hclust(d, method="single")
plot(h) # 2 clusters seems to be appropriate
c <- cutree(h, k = 2)
table(c,x[,3])
ggplot(x, aes(char, matches, color = c)) + 
  geom_point()

#Agglomerative on search radius and pickyness by gender [Better than k-Means!]
ggplot(datasetadj, aes(datasetadj[,4], datasetadj[,3], color = datasetadj[,2])) + geom_point()
x <- cbind(datasetadj[,4],datasetadj[,3], datasetadj[,2])
x <- scale(x)
x <- as.data.frame(x)
colnames(x) <- c("pickyness", "search_radius", "gender")
d <- dist(x, method="euclidean")
h <- hclust(d, method="single")
plot(h) # 2 clusters seems to be appropriate
c <- cutree(h, k = 2)
table(c,x[,3])
ggplot(x, aes(pickyness, search_radius, color = c)) + 
  geom_point()


#Kmeans on pickyness and matches by gender
x <- cbind(datasetadj[,4],datasetadj[,9], datasetadj[,2])
#x <- removeValue(x,1,0)
x <- scale(x)
x <- as.data.frame(x)
colnames(x) <- c("pickyness", "matches", "gender")

km <- lapply(1:6, function(k) replicate(10, kmeans(x, centers = k)$tot.withinss))
wss_k = sapply(1:6, function(k) mean(km[[k]]))

plot(x = 1:6, y = log10(wss_k),
     xlab = "k", ylab = expression("log(" * WSS[k] * ")"),
     las = 1, pch = 19, type = "o")

k <- kmeans(x, 2)
table(k$cluster, x$gender)
k$cluster <- as.factor(k$cluster)
ggplot(x, aes(pickyness, matches, color = k$cluster)) + geom_point()

#DBScan
x <- cbind(datasetadj[,4],datasetadj[,3])#, datasetadj[,2])
x <- scale(x)
x <- as.data.frame(x)
colnames(x) <- c("pickyness", "search_radius")#, "gender")
db <- dbscan(x, eps = .5, minPts = 4)
plot(db$cluster)

#plot tests
xaxis = 4
yaxis = 9

ggplot(datasetadj, aes(datasetadj[,xaxis], datasetadj[,yaxis], color = datasetadj[,2])) + geom_point() + labs(x = colnames(datasetadj)[xaxis], y = colnames(datasetadj)[yaxis])
pairs(datasetadj)

mean(datasetadj$`#characters_bio`[which(datasetadj$gender ==0)])
which(is.na(datasettest$gender))

nrow(datasettest)
datasettest = dataset[-c(6,229,348),]




#########main##########
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
#Clustering
#Showing number in bio - matches by gender
ggplot(datasetadj, aes(datasetadj[,5], datasetadj[,9], color = datasetadj[,2])) + geom_point() + labs(x = "#characters_bio", y="matches", colour="gender")


#ggplot(datasetadj, aes(datasetadj[,4], datasetadj[,5])) + geom_point()
#pairs(datasetadj)

pairs(datasetadj, col=ifelse(datasetadj$gender==0, "red", "blue"))



#PCA


