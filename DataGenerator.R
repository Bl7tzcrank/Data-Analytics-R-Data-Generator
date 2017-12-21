install.packages("chron");
install.packages("clusterGeneration")

library("clusterGeneration")
library("chron");

getTimes <- function(){
  t <- seq(ISOdate(2000,1,1,hour = 10), ISOdate(2000,12,31, hour =24), "hours");
  t[hours(t)>=10]
}

bimodalDistFunc <- function (n,cpct, mu1, mu2, sig1, sig2) {
  y0 <- rnorm(n,mean=mu1, sd = sig1)
  y1 <- rnorm(n,mean=mu2, sd = sig2)
  
  flag <- rbinom(n,size=1,prob=cpct)
  y <- y0*(1 - flag) + y1*flag 
}

sortbimodaldata = function(x){
  temp = x
  if(max(x) > 23){
    temp = temp[-which(temp>23)]
  } else if(min(temp)<10){
    temp = temp[-which(temp<10)]
  }
  return(temp)
}

createCustomerPerDay = function(n,cpct, mu1, mu2, sig1, sig2){
  data = bimodalDistFunc(n,cpct, mu1, mu2, sig1, sig2)
  floordata = floor(data)
  sorteddata = sortbimodaldata(floordata)
  return(sorteddata)
}

createCustomerTimeFrequency = function(n,cpct, mu1, mu2, sig1, sig2){
  allcustomers = as.data.frame(table(createCustomerPerDay(n,cpct, mu1, mu2, sig1, sig2)))
  freq = c()
  for (i in 10:23){
      if(length(which(allcustomers[,1] == i)) > 0){
        if (allcustomers[which(allcustomers[,1] == i),2] >60){
          freq = append(freq, 60)
        }else{
          freq = append(freq, allcustomers[which(allcustomers[,1] == i),2])
        }
      } else{
        freq = append(freq, 0)
      }
  }
  return(freq)
}

createAllCustomers = function(cpct, mu1, mu2, sig1, sig2){
  customers = c()
  for(i in 1:366){
    n = floor(runif(1, 150, 250))
    customers = append(customers, createCustomerTimeFrequency(n,cpct, mu1, mu2, sig1, sig2))
  }
  return(customers)
}

createMealsDrinks = function(timeslots,customers){
  meals = c()
  drinks = c()
  
  #uniform distribution
  #meals = c(runif(length(timeslots),7,12)*runif(length(timeslots),0.8,1.2),runif(length(timeslots),0,4)*runif(length(timeslots),0.8,1.2))*customers
  #drinks = c(runif(length(timeslots),0,4)*runif(length(timeslots),0.8,1.2),runif(length(timeslots),7,12)*runif(length(timeslots),0.8,1.2))*customers
  
  #random Cluster generation
  var = genRandomClust(numClust=2,sepVal=0.2,numNonNoisy=2,clustszind=1,clustSizeEq=2562,lambdaLow = 0.5,ratioLambda = 2)
  mealsDrinks=var$datList$test_1+min(var$datList$test_1)*-1
  
  meals=mealsDrinks[,1]*customers
  drinks=mealsDrinks[,2]*customers
  
  #normal distribution
  #meals = c(rnorm(length(timeslots),5,1),rnorm(length(timeslots),2,0.5))*customers
  #drinks = c(rnorm(length(timeslots),2,0.8),rnorm(length(timeslots),10,1.5))*customers
  
  #Transfermatrix
  #for (i in 1:length(timeslots)){
  #  ranges = MDRangeMatrix[which(MDRangeMatrix[,1]==timeslots[i]),]
  #  meals = append(meals,floor(customers[i]*(runif(1,ranges[2],ranges[3]))*(runif(1,0.6,1.4))))
  #  drinks = append(drinks,floor(customers[i]*(runif(1,ranges[4],ranges[5]))*(runif(1,0.6,1.4))))
  #}
  data.frame(meals,drinks)
}

#####################################################end of functions###########################################

#####################################################begin of data instances###########################################
mu1 <- 12  
mu2 <- 20
sig1 <- 1.5
sig2 <- 1.5
cpct <- 0.5 
n = floor(runif(1, 150, 250))

MDRangeMatrix = matrix(c(10,4,6,0,2,11,4,6,0,2,12,4,6,0,2,13,4,6,0,2,14,4,5,0,1,15,0,2,6,8,16,0,2,6,8,17,0,2,6,8,18,4,6,1,2,19,4,6,1,2,20,4,6,1,2,21,0,2,7,10,22,0,2,7,10,23,0,2,7,10), nrow = 14, ncol = 5, byrow = TRUE )

#####################################################end of data instances###########################################

#####################################################begin of main part###########################################

allcustomers = createAllCustomers(cpct, mu1, mu2, sig1, sig2)
times = getTimes()
timeandcustomers = data.frame(times, allcustomers)
timeslots = as.numeric(substr(timeandcustomers$times,12,13))


merke = createMealsDrinks(timeslots,timeandcustomers[,2])
plot(merke)
test = merke/timeandcustomers[,2]
test2 = test[which(complete.cases(test)),]
plot(test2)
dbscan::dbscan(test2,0.3)
