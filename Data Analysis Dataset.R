####R-Script for analysing the given dataset####

#Import libraries
install.packages("GGally")
library(GGally)

#########functions##########

#1. Data Pre-processing#
#1.1 Exploratory Data Analysis#

#1.2 Data Transformation
#turns non-numeric factors into numeric values, only if factor levels available
getnumeric <-function(data){
  for(i in 1:ncol(data)){
    if(!is.null(data[,i])){
      if(i == 1){
        new_data = data.frame(lapply(data[i], as.numeric), stringsAsFactors=FALSE)
      }else{
        new_data = cbind(new_data,data.frame(lapply(data[i], as.numeric), stringsAsFactors=FALSE))
      }
    }
  }
  return(new_data)
}

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
  normalizedDataSet = matrix(normalizedDataSet,nrow = nrow(data),ncol= ncol(data))
  #colnames(normalizedDataSet)<-colnames(data)
  
  allunivariateoutliersCalc = sort((which(normalizedDataSet>3.5)))
  
  return(allunivariateoutliersCalc)
}

#Calculate generalized squared distances and plots X2
detectoutx <- function(data){
  MeansCol <- apply(data,2, mean)
  CovMatrix = cov(data)
  d = apply(data, 1, function(x) t(x - MeansCol) %*% solve(CovMatrix) %*% (x-MeansCol))
  qc = qchisq((seq_len(nrow(data)) - 1/2) / nrow(data), df = 9)
  sort.d = sort(d)
  
  #1.3.5 Visualize the distances in a chi-square distribution
  plot(qc, sort.d, las = 1, pch = 19, xlab = expression(paste(chi[4]^ 2,"-Quantile")), ylab = "Ordered Distances", xlim = range(qc) * c(0, 1.1), ylim = range(sort.d) * c(0, 1.1))
  abline(h=23.59, col="red")
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

#This function removes the given entry numbers and their corresponding rows from a given dataset


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
  
  x = getnumeric(data)
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
  sapply(colnames(data), function(x){
    shapiro.test(as.numeric(data[[x]]))[1:2]
  })
}


#2. Dimensionality Reduction#

#3. Cluster Analysis#

#########main##########
#Dataset description
#We created a dataset adopting the flirting-app “Tinder”, but for slightly older people, naming it “Finder”. Each user creates a profile and is able to change some settings and upload pictures. Our dataset includes some of those settings, the number of pictures uploaded as well as a “picky factor” used by the providers of the app. In general, swiping right means that the user wants to match with the shown user, swiping left is the opposite. We are only implementing a heterosexual version with two genders, meaning that women are matched to men only and other way round. For a more detailled description of the features please see the according subsection.
#We generate data for the following features:
#Age
#Gender
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
dataset = naremoval(dataset)

#Outlier detection
pairs(dataset) #for detecting outliers visually
allBoxPlots = boxplot(log10(dataset)) #Visualization of outliers by using log10 boxplots
detectoutstand(dataset) #outlier detection outputting standardized values
detectoutx(dataset) #outlier detection outputting X2plot

#Outlier removal

#Testing for normal distribution
normaldistplots(getnumeric(dataset)) #visually by QQ-Plots
mnormdistplots(getnumeric(dataset)) #visually by QQ-Plot
histo(getnumeric(dataset)) #visually by histograms
hytest(getnumeric(dataset)) #Shapiro-Wilk test

#Clustering
#Showing buyer_annual_income - article_price by article_group
ggplot(corrected_dataset, aes(buyer_annual_income, article_price, color = article_group)) + geom_point()
ggplot(corrected_dataset, aes(buyer_age, buyer_annual_income, color = article_group)) + geom_point()

