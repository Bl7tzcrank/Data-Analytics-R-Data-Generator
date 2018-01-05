install.packages("chron")
install.packages("clusterGeneration")
install.packages("lubridate")
install.packages("dbscan")

#install.packages("plotly")
#install.packages("ggplot2")


library("clusterGeneration")
library("dbscan")
library("lubridate")
library("chron")
#library("ggplot2")
#library("plotly")

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

decimalnumcount<-function(y){
  if(y%%1 != 0){
    x = toString(y)
    stopifnot(class(x)=="character")
    x<-gsub("(.*)(\\.)|([0]*$)","",x)
    nchar(x)
  }
  else{
    return(0)
  }
}

#cut off the values which are not in the range of 10 and 23
filterbimodaldata = function(x){ 
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
  sorteddata = filterbimodaldata(floordata) #cut off data in range 10 to 23
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
  
  meals=round(mealsDrinks[,1]*customers,0)
  drinks=round(mealsDrinks[,2]*customers,0)
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
  #avgmeals = unlist(sapply(1:NROW(data), function(x){
  #  if(customers[x] == 0){
  #    return(0)
  #  } else {
  #    return(data[x,1]/customers[x])
  #  }
  #}))
  
  #avgdrinks = unlist(sapply(1:NROW(data), function(x){
  #  if(customers[x] == 0){
  #    return(0)
  #  } else {
  #    return(data[x,2]/customers[x])
  #  }
  #}))
  
  return(data)
}

#creates the season (1-4) for every timestamp and returns it as a vector
#1: spring; 2: summer; 3: autumn; 4: winter
createSeason = function(dates){
  moy <- month(dates)
  soy <- sapply(moy, function(x){
    if (2 < x && x < 6){1}
    else if (5 < x && x < 9){2}
    else if (8 < x && x < 12){3}
    else if(11 < x || x < 3){4}
    })
  return (unlist(soy))
}

#create the temperature outside based on seasons 1-4, 14 hours a day
createWeather = function(){
  #calculates the average temperature of every month
  #we are assuming that the average temperature scaled from mai to april is distributed
  #like a sin(x)-function with mean of 11.5 and a factor of 12.5 to simulate
  #temperatures between -1 and 24 ?C
  monthlyavgtemperature = sapply(1:12, function(x){
    if(x < 5){
      11.5 - 12.5*sin(pi*((x+8)/6)-pi)
    } else{
      11.5 - 12.5*sin(pi*((x-4)/6)-pi)
    }
  })
  
  #calculates the average temperature of each day.
  #The temperature of a day is within an interval of the average 
  #temperature of the corresponding month +/- 3. (unif. distribution)
  dailyavgtemperature = unlist(sapply(1:12, function(x){
    if(x == 4 || x == 6 || x == 9 || x == 11){
      sapply(1:30, function(y){
        runif(n = 1, min = monthlyavgtemperature[x] - 3, max = monthlyavgtemperature[x] + 3)
      })
    } else if(x == 2){
      sapply(1:29, function(y){
        runif(n = 1, min = monthlyavgtemperature[x] - 3, max = monthlyavgtemperature[x] + 3)
      })
    } else{
      sapply(1:31, function(y){
        runif(n = 1, min = monthlyavgtemperature[x] - 3, max = monthlyavgtemperature[x] + 3)
      })
    }
  }))
  
  #calculates the avg. temperature on each hour of all days.
  #again modeled as a sin(x)-function with the avg dailytemp. as a mean
  #and +- 4 as the deviation.
  hourlyavgtemperature = unlist(as.list(sapply(1:366, function(x){
    sapply(1:24, function(y){
      dailyavgtemperature[x] - 3*sin(pi*((y)/12)-pi) #WIP
    })[1:14]
  })))
  
  return(hourlyavgtemperature)                    
}

#creates number of times the door was opened
createOpendoors = function(customers){
  
  opendoors <- sapply(customers,function(x){
    
    (abs(round(rnorm(1,mean=x, sd = 2)))+1)*2
    
    })
  return (opendoors)
}

#computes the amount of gas used per hour (kwh); depends on number of meals; input mealsanddrinks[,1]
createGas = function(numberofmeals){
  
  gas <- sapply(numberofmeals,function(x){
    
    (abs(round(rnorm(1,mean=x*500,sd=1000))))^(1/2)
  })
  
  gas <- round(gas,2)
  return(gas)
}


#generates the average age of the customers for each hour.
#Assumption: younger people visit our restaurant rather late in comparison to older people.
createAverageAge = function(customers){
  numberofentries = NROW(customers)
  
  return(unlist(sapply(1:numberofentries, function(x){
    if(customers[x] == 0){
      return(NA)  
    }
    
    if(x%%14 == 0){
      return(floor(mean(rnorm(customers[x],mean=25, sd = sqrt(10)))))
    } 
    else if(0 < x%%14 && x%%14 < 5){
      return(floor(mean(rnorm(customers[x],mean=50, sd = sqrt(35)))))
    } 
    else if(4 < x%%14 && x%%14 < 9){
      return(floor(mean(rnorm(customers[x],mean=25, sd = sqrt(10)))))
    } 
    else if(8 < x%%14 && x%%14 < 12){
      return(floor(mean(rnorm(customers[x],mean=40, sd = sqrt(15)))))
    } 
    else if(11 < x%%14 && x%%14 < 14){
      return(floor(mean(rnorm(customers[x],mean=30, sd = sqrt(15)))))
    }
  })))
}

#creates the number of paymentmethods per hour. Assumption:
#older people tend to pay cash and younger people with cards.
createPaymentMethods = function(customers, customerages){
  maxAge = max(customerages[-which(is.na(customerages))])
  card = c()
  cash = c()
  
  for(i in 1:length(customers)){
    if(customers[i] == 0){
      card = append(card, 0)
      cash = append(cash, 0)
    } else {
      weight = customerages[i]/maxAge
      if(weight > 0.2){
        weight = weight - 0.2
      }
      card0_cash1 = rbinom(customers[i],size=1,prob=weight)
      card = append(card, length(card0_cash1[which(card0_cash1 == 0)]))
      cash = append(cash, length(card0_cash1[which(card0_cash1 == 1)]))
    }
  }
  
  return(data.frame("card" = card, "cash" = cash))
}

#computes the ampunt of water used per hour (liters); depends on number of meals; input mealsanddrinks[,1]
createWater = function(numberofmeals){
  water <- sapply(numberofmeals,function(x){
    (abs(round(rnorm(1,mean=x*10,sd=10))))^(2/3)
  })
  water <- round(water,2)
  return(water)
}

#computes the ampunt of electricity used per hour (kwh); depends on number of meals and weather outside; input mealsanddrinks[,1]
createElectricity = function(numberofmeals,weather){

  electricitycooking <- sapply(numberofmeals,function(x){
    
    (abs(round(rnorm(1,mean=x*100000,sd=500000))))^(1/3)
  })
  # 1 kwh per degree lower then 20
  electricityheating <- sapply(weather, function(x){
    if(x < 20){
      (20-x)*1
    }else{0}
  })
  
  electricity <- round(electricitycooking+electricityheating,2)
  
  return (electricity)
}

#calculates restaurant temperature
#Reasoning: The temperature restaurant depends on the number of guests and how often the door was opened.
#The assumption is, that the if the door is opened, there is a negative effect on the restaurant temperature. Therefore the temperature decreases
#initially. The more people the more does the temperature increase because the body heat (and also cooked meals) overrules the negative effect of
#the door.
#IDEA:How about also including the season. Depending on that there might be a positive or negative effect if the door is opened
get_restaurant_temperature = function(number_of_customers,doors_opened){
  temperature_cust = x1*number_of_customers^3 - x2*number_of_customers^2 + number_of_customers/5 + fixed_temperature_for_cust
  temperature_OD = y1*doors_opened^2 + fixed_temperature_for_opened_doors
  temperature = temperature_cust*(1-weight_factor_opened_doors)+temperature_OD*weight_factor_opened_doors
  return(round(temperature,1))
}

#calculates tips_earned
#Reasoning:The tips of course heavily depend on the number of customers. In addition the older the customers, the higher the average tip.
get_tips = function(number_of_customers,average_age){
  averageAge[which(is.na(averageAge))] = 0
  tips = round((rnorm(length(number_of_customers),average_tip,variance_tip)+additional_tips_average_age*(average_age/max_age)),2)*number_of_customers 
}
  
#calculates revenue
#Reasoning: Revenues depend on consumed meals and drinks. Furthermore customers paying with cards tend to order more expensive meals than customers
#paying cash.
get_revenues = function(number_of_meals, number_of_drinks, card_payments,cash_payments){
  revenue_meals = rnorm(length(number_of_meals),average_price_meal,variance_meal_price)*number_of_meals
  revenue_drinks = rnorm(length(number_of_drinks),average_price_drink,variance_drink_price)*number_of_drinks
  
  paymentbinaer = apply(data.frame(card_payments, cash_payments, rep(1,times=length(cash_payments))),1,function(X){
    if(X[1]>X[2]){
      X[3]=1
    }else{X[3]=0}
  })
  
  temp = data.frame(cash_payments,card_payments,revenue_meals,revenue_drinks,paymentbinaer)

  revMeals = apply(temp[,3:5],1,function(X){
    if(X[3]==1){  
      X[1] = X[1] * runif(1,min_revenue_percentage_boost,max_revenue_percentage_boost)
    }else{
      X[1]=X[1]
    }
  })
  
  revDrinks = apply(temp[,3:5],1,function(X){
    if(X[3]==1){  
      X[2] = X[2] * runif(1,min_revenue_percentage_boost,max_revenue_percentage_boost)
    }else{
      X[2]=X[2]
    }
  })
  
  result = data.frame(round(revMeals+revDrinks,2),round(revMeals,2),round(revDrinks,2))
  colnames(result) <- c("total_revenue","meal_revenue","drink_revenue")
  return(result)
}

#1%-chance is stays in data range, 1%-chance that is doesnt stay in data range
createoutliers <- function(dataset){
  
  for(k in 3:ncol(dataset)){
    
    minX <- min(dataset[k],na.rm=TRUE)
    maxX <- max(dataset[k],na.rm=TRUE)
    
    for (i in 1:length(dataset[,k])){
      
      if(!is.na(dataset[i,k])){
        random <- round(runif(1,1,1000))
        if(random == 999){
          #for outliers within the data range
          
          #print(paste0("Min: ", minX))
          #print(paste0("Max: ", maxX))
          
          randomoutlier = runif(1,minX,maxX)
          
          adaptedoutlier = trunc(randomoutlier*10^decimalnumcount(maxX))/10^decimalnumcount(maxX)
          #print(paste0("Original: ", dataset[i,k]))
          dataset[i,k] = adaptedoutlier
          
          #print(paste0("Outlier1: ", adaptedoutlier))
          #print(paste0("Outlier2: ", dataset[i,k]))
          
        }#else if(random == 1000){
          #for outliers without the data range
          #random <- round(runif(1,1,2))
          
          #print(paste0("ExtremeOriginal: ", dataset[i,k]))
          #if(random == 1){
            #smaller
            #randomoutlier = rnorm(1,minX+maxX,minX+maxX)
            #adaptedoutlier = trunc(randomoutlier*10^decimalnumcount(minX))/10^decimalnumcount(minX)
            #dataset[i,k] = minX - adaptedoutlier
          #}else{
            #bigger
            #randomoutlier = rnorm(1,minX+maxX,minX+maxX)
            #adaptedoutlier = trunc(randomoutlier*10^decimalnumcount(maxX))/10^decimalnumcount(maxX)
            #dataset[i,k] = maxX + adaptedoutlier
          }
          #print(paste0("Min: ", minX))
          #print(paste0("Max: ", maxX))
          #print(paste0("Random: ", randomoutlier))
          #print(paste0("ExtremeDifference: ", adaptedoutlier))
          #print(paste0("ExtremeOutlier: ", dataset[i,k]))
        #}
      #}
     
    }
  }
  
  return(dataset)
}

#####################################################end of functions###########################################

#####################################################begin of data instances###########################################
mu1 <- 12  
mu2 <- 20
sig1 <- 1.5
sig2 <- 1.5
cpct <- 0.5 
n = floor(runif(1, 150, 250))

#parameters_restaurant_temperature
fixed_temperature_for_cust = 18.0
fixed_temperature_for_opened_doors = 23.0
weight_factor_opened_doors = 0.55
x1 = 0.001
x2 = 0.025
y1 = -0.0025

#parameters_tips
average_tip = 2.5
variance_tip = 0.5
additional_tips_average_age = 3
max_age = 60

#parameters_revenue
average_price_meal = 8.0
variance_meal_price = 2.0
average_price_drink = 4.0
variance_drink_price = 1.0
max_revenue_percentage_boost = 1.4
min_revenue_percentage_boost = 1.3

MDRangeMatrix = matrix(c(10,4,6,0,2,11,4,6,0,2,12,4,6,0,2,13,4,6,0,2,14,4,5,0,1,15,0,2,6,8,16,0,2,6,8,17,0,2,6,8,18,4,6,1,2,19,4,6,1,2,20,4,6,1,2,21,0,2,7,10,22,0,2,7,10,23,0,2,7,10), nrow = 14, ncol = 5, byrow = TRUE )

#####################################################end of data instances###########################################

#####################################################begin of main part###########################################

times = getTimes()
allcustomers = createAllCustomers(cpct, mu1, mu2, sig1, sig2)
season = createSeason(times)
timeandcustomers = data.frame(times, allcustomers)
#timeslots = as.numeric(substr(timeandcustomers$times,12,13))
averageAge = createAverageAge(allcustomers)
tips=get_tips(timeandcustomers[,2],averageAge)
doors_opened = createOpendoors(timeandcustomers[,2])
outsidetemperature = createWeather()
restaurant_temperature = get_restaurant_temperature(timeandcustomers[,2],doors_opened)
mealsanddrinks = createMealsDrinks(timeandcustomers[,2])
gas_consumption = createGas(mealsanddrinks[,1])
water_consumption = createWater(mealsanddrinks[,1])
electricity_consumption = createElectricity(mealsanddrinks[,1],outsidetemperature)
paymentMethods = createPaymentMethods(timeandcustomers[,2],averageAge)
revenues = get_revenues(mealsanddrinks[,1], mealsanddrinks[,2], paymentMethods[,1], paymentMethods[,2])

dataset1 = data.frame("time" = times, "season" = season, "customers" = allcustomers, "average_Age" = averageAge, 
                     "tips" = tips, "doors_opened" = doors_opened, "weather" = outsidetemperature, "restaurant_temperature" = restaurant_temperature, 
                     mealsanddrinks, "gas_consumption" = gas_consumption, "water_comsumption" = water_consumption, 
                     "electricity_consumption" = electricity_consumption, paymentMethods, revenues)

dataset2 <- createoutliers(dataset1)

