install.packages("chron")
install.packages("clusterGeneration")
install.packages("lubridate")
install.packages("dbscan")


library("clusterGeneration")
library("dbscan")
library("lubridate")
library("chron")

getTimes <- function(){
  t <- seq(ISOdate(2000,1,1,hour = 10), ISOdate(2000,12,31, hour =24), "hours")
  t[hours(t)>=10]
}

#creates bimodal random values by merging two sets of randonly normal distributed values
#n = number of values, cpct = 0.5 (probability of choosing of one of the sets)
bimodalDistFunc <- function (n,cpct, mu1, mu2, sig1, sig2) {
  y0 <- rnorm(n,mean=mu1, sd = sig1)
  y1 <- rnorm(n,mean=mu2, sd = sig2)
  
  flag <- rbinom(n,size=1,prob=cpct)
  y <- y0*(1 - flag) + y1*flag 
}

#cut off the values which are not in the range of 10 and 23
sortbimodaldata = function(x){ 
  temp = x
  if(max(x) > 23){
    temp = temp[-which(temp>23)]
  } else if(min(temp)<10){
    temp = temp[-which(temp<10)]
  }
  return(temp)
}

#number of customers per day; e.g. (12,12,15,16,11,...)
createCustomerPerDay = function(n,cpct, mu1, mu2, sig1, sig2){
  data = bimodalDistFunc(n,cpct, mu1, mu2, sig1, sig2)
  floordata = floor(data) #round down
  sorteddata = sortbimodaldata(floordata) #cut off data in range 10 to 23
  return(sorteddata)
}

#output: 14 rows of distribution for one day (10,20,21,44,75,54,...) without the time. Maximum capacity of 60.
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

#Doing the previous function 365 times. Returns the whole customers data (365 days and 14 entries a day)
createAllCustomers = function(cpct, mu1, mu2, sig1, sig2){
  customers = c()
  for(i in 1:366){
    n = floor(runif(1, 150, 250)) #randomly choose a number of customer for a day
    customers = append(customers, createCustomerTimeFrequency(n,cpct, mu1, mu2, sig1, sig2))
  }
  return(customers)
}

createMealsDrinks = function(customers){ #parameters are not necessary (?)
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
  data = data.frame(meals, drinks)
  #normal distribution
  #meals = c(rnorm(length(timeslots),5,1),rnorm(length(timeslots),2,0.5))*customers
  #drinks = c(rnorm(length(timeslots),2,0.8),rnorm(length(timeslots),10,1.5))*customers
  
  #Transfermatrix
  #for (i in 1:length(timeslots)){
  #  ranges = MDRangeMatrix[which(MDRangeMatrix[,1]==timeslots[i]),]
  #  meals = append(meals,floor(customers[i]*(runif(1,ranges[2],ranges[3]))*(runif(1,0.6,1.4))))
  #  drinks = append(drinks,floor(customers[i]*(runif(1,ranges[4],ranges[5]))*(runif(1,0.6,1.4))))
  #}
  avgmeals = unlist(sapply(1:NROW(data), function(x){
    if(customers[x] == 0){
      return(0)
    } else {
      return(data[x,1]/customers[x])
    }
  }))
  
  avgdrinks = unlist(sapply(1:NROW(data), function(x){
    if(customers[x] == 0){
      return(0)
    } else {
      return(data[x,2]/customers[x])
    }
  }))
  
  return(data.frame(avgmeals, avgdrinks))
}

#creates the season (1-4) for every timestamp and returns it as a vector
createseason = function(dates){
  moy <- month(dates)
  soy <- sapply(moy, function(x){
    if (2 < x && x < 6){1}
    else if (5 < x && x < 9){2}
    else if (8 < x && x < 12){3}
    else if(11 < x || x < 3){4}
    })
  return (unlist(soy))
}

#creates number of times the door was opened
createOpendoors = function(customers){
  
  opendoors <- sapply(customers,function(x){
    
    (abs(round(rnorm(1,mean=x, sd = 2)))+1)*2
    
    })
  return (opendoors)
}

#computes the amount of gas used per hour (kwh); depends on numberofmeals*numberofcustomers; input mealsanddrinks[,1]
createGas = function(numberofmeals,customers){
  
  gas <- mapply(function(x,y){
    
    (abs(round(rnorm(1,mean=x*y*500,sd=1000)))+1)^(1/2)
  }, numberofmeals,customers)
  
  return(gas)
}

createWater = function(numberofmeals,customers){
  
  water <- mapply(function(x,y){
    
    (abs(round(rnorm(1,mean=x*y*10,sd=10)))+1)^(2/3)
  }, numberofmeals,customers)
  
  return(water)
  
}

<<<<<<< HEAD
#calculates restaurant temperature
#OPEN:adjustment of 
get_restaurant_temperature = function(number_of_customers,doors_opened){
  temperature = x1*number_of_customers^3 - x2*number_of_customers^2 - y1*doors_opened^2 + minimum_temperature
  return(temperature)
}

=======
>>>>>>> 3908ee03204623e0ff66d1879fd9f79ecf222ba9
#####################################################end of functions###########################################

#####################################################begin of data instances###########################################
mu1 <- 12  
mu2 <- 20
sig1 <- 1.5
sig2 <- 1.5
cpct <- 0.5 
n = floor(runif(1, 150, 250))
#parameters_restaurant_temperature
minimum_temperature = 18.0
x1 = 0.001
x2 = 0.01
y1 = 0.1

MDRangeMatrix = matrix(c(10,4,6,0,2,11,4,6,0,2,12,4,6,0,2,13,4,6,0,2,14,4,5,0,1,15,0,2,6,8,16,0,2,6,8,17,0,2,6,8,18,4,6,1,2,19,4,6,1,2,20,4,6,1,2,21,0,2,7,10,22,0,2,7,10,23,0,2,7,10), nrow = 14, ncol = 5, byrow = TRUE )

#####################################################end of data instances###########################################

#####################################################begin of main part###########################################

allcustomers = createAllCustomers(cpct, mu1, mu2, sig1, sig2)
times = getTimes()
timeandcustomers = data.frame(times, allcustomers)
#timeslots = as.numeric(substr(timeandcustomers$times,12,13))

plot(get_restaurant_temperature(timeandcustomers[,2],timeandcustomers[,2]))

mealsanddrinks = createMealsDrinks(timeandcustomers[,2])
