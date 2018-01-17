####R-Script for analysing the given dataset####

####Data set description###
# Data Generator for the present use-case
# The idea is to create a dataset of bought presents in 2017 in the "Münsterland"
# ------------------------- list of features --------------------- #
# buyer_id -> different people buy different things, some of them buy more than one present, therefore, they need an identifier
# buyer_gender-> maybe there are differents in gender
# buyer_age-> older -> higher mean income -> more expensive presents
# buyer_annual_income -> the more income, the more expensive the presents
# article-> more a subgroup to specifiy what was bought
# article_group-> group, includes multiple subgroups
# article_price -> price of the present
# orderdate-> maybe more presents bought before christmas and easter
# packaging -> was the present packaged?
# shop -> online vs store
# city -> where was it bought (store location or delvery "address")
# recipient_age -> age of recipient
# recipient_gender -> gender of recipient
# recipient_relation -> it is important if you by a present for your grandma or your boy-/girlfriend

#Read data set
data_set_17 = read.csv("Datasets/data_17.csv")

#Import libraries
install.packages("GGally")
library(GGally)
#1. Data Pre-processing#

#1.1 Exploratory Data Analysis#
str(data_set_17)
summary(data_set_17)
#ggpairs(data_set_17[-c(1,3,11,15)])

#1.2 Data Transformation
#turns non-numeric factors into numeric values
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
pairs(data_set_17) #for detecting outliers visually

#1.3.1 Identification of univariate outliers using box plots

#Reduction of dataset because not all features can be checked with boxplots(Data Type)
#Remaining features: buyer_age, buyer_annual_income, article_price, recipient_age
adjDataSet = data_set_17[,c(4,6,12,13)]

#Visualization of outliers by using log10 boxplots
allBoxPlots = boxplot(log10(adjDataSet))

#Check of article_price and recipient_age outliers using normal boxplots to verify outliers below the minimum
checkBoxplots_article_price = boxplot(adjDataSet$article_price)
checkBoxplots_recipient_age = boxplot(adjDataSet$recipient_age)
#The result of the check is that there are not outliers below the minimum. However those above the maximum are still valid.

#Use of max values of Boxplots in order to retrieve all outliers
checkVector = allBoxPlots$stats[5,]
allunivariateoutliersVisual = sort(c(which(log10(adjDataSet$buyer_age)>checkVector[1]), which(log10(adjDataSet$buyer_annual_income)>checkVector[2]),which(log10(adjDataSet$article_price)>checkVector[3]), which(log10(adjDataSet$recipient_age)>checkVector[4])))

#1.3.2 Identification of bivariate outliers using scatter plots


#1.3.3 Calculate standardized values
MeansCol <- apply(adjDataSet,2, mean)
CovMatrix = cov(adjDataSet)
SDCol = sqrt(diag(CovMatrix))
temp = sweep(adjDataSet,2,MeansCol)
normalizedDataSet = apply(temp,1,function(x){
  x/SDCol
})

#Formating of results
normalizedDataSet = matrix(normalizedDataSet,nrow = 2000,ncol = 4)
colnames(normalizedDataSet)<-c("buyer_age","buyer_annual_income","article_price","recipient_age")
allunivariateoutliersCalc = sort(ceiling(which(normalizedDataSet>3.5)/4))

#Check if calculated outliers are equal to those identified using boxplots
if(all(allunivariateoutliersCalc == allunivariateoutliersVisual)){
  print("Visual outliers confirmed by standardized values.")
  outliers = allunivariateoutliersCalc
}else{
  print("Visual outliers differ from outliers identified by standardized values. Please check.")
}

#1.3.4 Calculate generalized squared distances
d = apply(adjDataSet, 1, function(x) t(x - MeansCol) %*% solve(CovMatrix) %*% (x-MeansCol))
qc = qchisq((seq_len(nrow(adjDataSet)) - 1/2) / nrow(adjDataSet), df = 4)
sort.d = sort(d)

#1.3.5 Visualize the distances in a chi-square distribution
plot(qc, sort.d, las = 1, pch = 19, xlab = expression(paste(chi[4]^ 2,"-Quantile")), ylab = "Ordered Distances", xlim = range(qc) * c(0, 1.1), ylim = range(sort.d) * c(0, 1.1))
abline(h=14.86, col="red")
#Zoom in to check if points are below threshold
plot(qc, sort.d, las = 1, pch = 19, xlab = expression(paste(chi[4]^ 2,"-Quantile")), ylab = "Ordered Distances", xlim = c(0,20), ylim = c(0,20))
abline(h=14.86, col="red")


allunivariateoutliersChiSquare = which(d>14.86)

if(all(allunivariateoutliersChiSquare == outliers)){
  print("Outliers confirmed by Chi Squared.")
}else{
  print("Chi squared outliers differ from previous outliers. Please check.")
}


#1.4 Outlier removal#
corrected_dataset = data_set_17

outliers_data_sets = data_set_17[outliers,]

#Check if there are more entries of the same customer to correct buyer_age and buyer_annual_income
data_set_17[data_set_17$buyer_id %in% outliers_data_sets$buyer_id,]

#Correct buyer_age data by using values of other data sets
corrected_dataset[36,4]=data_set_17[9,4]
corrected_dataset[1214,4]=data_set_17[1629,4]

#Correct buyer_age data by using rounded average values of age (from data without the extreme values)
#corrected_dataset[1644,4]=floor(mean(corrected_dataset$buyer_age[-c(1644,1820)]))#this has to be 72 according to dependency between buyer_id and buyer_age
corrected_dataset[1644,4]= 72
#corrected_dataset[1820,4]=floor(mean(corrected_dataset$buyer_age[-c(1644,1820)])) #this has to be 25 or 26 according to dependency between buyer_id and buyer_age
corrected_dataset[1644,4]= 25

#Correct article price by the "missing" value. REVIEW required! Only temporal solution
data_set_17$article_price[which(data_set_17$article=="TV serial - DVD")]
plot(data_set_17$article_price[which(data_set_17$article=="TV serial - DVD")])
#44.99
corrected_dataset[193,12]=44.99

#Correct article price by the "missing" value. REVIEW required! Only temporal solution
data_set_17$article_price[which(data_set_17$article=="Perfume")]
plot(data_set_17$article_price[which(data_set_17$article=="Perfume")])
#228.99
corrected_dataset[1488,12]=228.99

#Correct reciepient_age by using average age of all other customers.
avgRecAge = mean(data_set_17$recipient_age[-c(1545,1586)])

corrected_dataset[1545,13] = avgRecAge
corrected_dataset[1586,13] = avgRecAge

#No values for buyer_annual_income. As we cannot assume that those values are incorrect (annual incomes of 354000 and 494000 are possible)
#we decided to exclude the data sets in order to facilitate further analysis.
corrected_dataset = corrected_dataset[-c(1159,1178),]

pairs(corrected_dataset)

ggpairs(corrected_dataset[-c(3,11,15)])

plot(corrected_dataset$buyer_id,corrected_dataset$buyer_age)

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

#Testing for normal distribution
normaldistplots(getnumeric(data_set_17)) #visually by QQ-Plots
mnormdistplots(getnumeric(data_set_17)) #visually by QQ-Plot
histo(getnumeric(data_set_17)) #visually by histograms
hytest(getnumeric(data_set_17)) #Shapiro-Wilk test



