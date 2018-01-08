#################### BEGIN PACK&LIB ####################
install.packages("chron")
install.packages("clusterGeneration")
install.packages("lubridate")

library("clusterGeneration")
library("lubridate")
library("chron")
####################  END PACK&LIB  ####################

#################### BEGIN FUNCTIONS ####################

###Functions related to customer creation###

#Description: Provides the time dimension for the data set. To be more specific the function provides dates for one year, each date having 14 hours (working hours)
#Input: none
#Output: Time vector
getTimes <- function(){
  t <- seq(ISOdate(AnalyzedYear,1,1,hour = startOfWork), ISOdate(AnalyzedYear,12,31, hour = endOfWork), "hours")
  t[hours(t)>=startOfWork]
}

#Description: Creates bimodal random values by merging two sets of randonly normal distributed values to simulate two peaks (lunch,dinner).
#Input: n = number of values, cpct = 0.5 (probability of choosing of one of the sets), mu1 = time of lunch peack, mu2 = time of dinner peak, sig1 = variance of lunch time
#sig2 = variance of dinner time
#Output: Initial number of customers (not assigned to a specific point in time)
bimodalDistFunc <- function (n,cpct, mu1, mu2, sig1, sig2) {
  y0 <- rnorm(n,mean=mu1, sd = sig1)
  y1 <- rnorm(n,mean=mu2, sd = sig2)
  
  flag <- rbinom(n,size=1,prob=cpct)
  y <- y0*(1 - flag) + y1*flag 
}

#Description: Counts the number of decimals
#Input: y = numeric value to be analyzed
#Output: Decimal count
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

#Description: Cut off the values which are not in the range of 10 and 23 (meaning not in the working hours)
#Input: x = data of the bimodal customer data set
#Output: Cleaned customer data set
filterbimodaldata = function(x){ 
  temp = x
  if(max(x) > (endOfWork-1)){
    temp = temp[-which(temp>(endOfWork-1))]
  } else if(min(temp)<startOfWork){
    temp = temp[-which(temp<startOfWork)]
  }
  return(temp)
}

#Description: Creates number of customers per day; e.g. (12,12,15,16,11,...)
#Input: n = number of values, cpct = 0.5 (probability of choosing of one of the sets), mu1 = time of lunch peack, mu2 = time of dinner peak, sig1 = variance of lunch time
#sig2 = variance of dinner time
#Output: Customer data set
createCustomerPerDay = function(n,cpct, mu1, mu2, sig1, sig2){
  data = bimodalDistFunc(n,cpct, mu1, mu2, sig1, sig2)
  floordata = floor(data)
  sorteddata = filterbimodaldata(floordata)
  return(sorteddata)
}

#Description: Creates all customers and checks whether restriction "capacity =< 60" has been violated.
#Input: n = number of values, cpct = 0.5 (probability of choosing of one of the sets), mu1 = time of lunch peack, mu2 = time of dinner peak, sig1 = variance of lunch time
#sig2 = variance of dinner time
#Output: 14 rows of distribution for one day (10,20,21,44,75,54,...) without the time.
createCustomerTimeFrequency = function(n,cpct, mu1, mu2, sig1, sig2){
  allcustomers = as.data.frame(table(createCustomerPerDay(n,cpct, mu1, mu2, sig1, sig2)))
  freq = c()
  for (i in 10:23){
    if(length(which(allcustomers[,1] == i)) > 0){
      if (allcustomers[which(allcustomers[,1] == i),2] >maxCapacity){
        freq = append(freq, maxCapacity)
      }else{
        freq = append(freq, allcustomers[which(allcustomers[,1] == i),2])
      }
    } else{
      freq = append(freq, 0)
    }
  }
  return(freq)
}
#Description: Returns the whole customers data set (365 days and 14 entries a day)
#Input: n = number of values, cpct = 0.5 (probability of choosing of one of the sets), mu1 = time of lunch peack, mu2 = time of dinner peak, sig1 = variance of lunch time
#sig2 = variance of dinner time
#Output: Number of customers per hour
createAllCustomers = function(cpct, mu1, mu2, sig1, sig2){
  customers = c()
  for(i in 1:366){
    n = floor(runif(1, minCustomerPerDay, maxCustomerPerDay)) #randomly choose a number of customer for a day
    customers = append(customers, createCustomerTimeFrequency(n,cpct, mu1, mu2, sig1, sig2))
  }
  return(customers)
}

###End of Functions related to customer creation###

#Description: Creates the number of meals and drinks per hour based on random cluster generator and number of customers. 
#The attributes are clustered, indicating that there are mainly two kinds of customers in terms of drinking/eating behaviour
#Input: customers = customer vector
#Output: Number of drinks and meals per hour
createMealsDrinks = function(customers){
  meals = c()
  drinks = c()
  
  var = genRandomClust(numClust=2,sepVal=0.2,numNonNoisy=2,numNoisy=1,clustszind=1,clustSizeEq=2562,lambdaLow = 0.5, ratioLambda = 2)
  mealsDrinks=var$datList$test_1+min(var$datList$test_1)*-1
  
  meals=round(mealsDrinks[,1]*customers,0)
  drinks=round(mealsDrinks[,2]*customers,0)
  data = data.frame(meals, drinks)
  
  return(data)
}

#Description: Creates the season (1-4) for every timestamp and returns it as a vector.
#The attributes are clustered, indicating that there are mainly two kinds of customers in terms of drinking/eating behaviour
#Input: Timestamps
#Output: Numeric value for season (1: spring; 2: summer; 3: autumn; 4: winter)
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

#Description: Turns season into string data type
#Input: Numeric value for season
#Output: Non-numeric value for season (1: spring; 2: summer; 3: autumn; 4: winter)
createSeason_nonnumeric = function(season){
  sapply(season,function(x){
    if (x == 1){"Spring"}
    else if (x == 2){"Summer"}
    else if (x == 3){"Autumn"}
    else if(x == 4 ){"Winter"}
  })
}

#Description: Create the temperature outside based on seasons 1-4, 14 hours a day
#Input: None
#Output: Vector with outside temperatur values for every hour of the year.
createWeather = function(){
  #calculates the average temperature of every month
  #we are assuming that the average temperature scaled from mai to april is distributed
  #like a sin(x)-function with mean of 11.5 and a factor of 12.5 to simulate
  #temperatures between -1 and 24 C
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
  
  return(round(hourlyavgtemperature,1))                    
}

#Description: Creates number of times the door was opened depending on number of customers and season
#Input: customers = vector of customer per hour, season = vector of season per hour
#Output: Vector with values for times the door has been opened within one hour.
createOpendoors = function(customers,season){
  
  opendoors <- mapply(function(x,y){
    if (y == 4){b = 1}
    else {b = y}
    (abs(round(rnorm(1,mean=x*(b/2), sd = 2)))+1)*2
  },customers,season)
  
  return (opendoors)
}

#Description: Computes the amount of gas used per hour (kwh) based on number of meals
#Input: numberofmeals = number of meals per hour
#Output: Vector with gas consumption per hour.
createGas = function(numberofmeals){
  
  gas <- sapply(numberofmeals,function(x){
    
    (abs(round(rnorm(1,mean=x*g_mean_factor,sd=g_sd))))^(g_curve)
  })
  
  gas <- round(gas,2)
  return(gas)
}

#Description: Generates the average age of the customers for each hour.
#Assumption is that younger people visit our restaurant rather late in comparison to older people.
#Input: customers = vector of customer per hour
#Output: Vector with average age of customers in the restaurant per hour.
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

#Description: Creates the number of paymentmethods per hour. 
#Assumption is that older people tend to pay cash and younger people with cards.
#Input: customers = vector of customer per hour, customerages = average age of customers per hour
#Output: Vector of card and cash transactions in the restaurant per hour.
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

#Description: Computes the amount of water used per hour (liters)
#It depends on number of meals and weather (sprinkler)
#Input: numberofmeals = number of meals per hour, weather = outside temperature
#Output: Vector with water consumption data in the restaurant per hour.
createWater = function(numberofmeals,weather){
  water <- mapply(function(x,y){
    bw=1
    if(y>20){
      bw=1000*(y-20)
    }
    (abs(round(rnorm(1,mean=x*w_mean_factor,sd=w_sd)+bw)))^(w_curve)
  },numberofmeals,weather)
  water <- round(water,2)
  return(water)
}

#Description: Computes the amount of electricity used per hour (kwh)
#It depends on number of meals and weather outside
#Input: numberofmeals = number of meals per hour, weather = outside temperature
#Output: Vector with power consumption data in the restaurant per hour.
createElectricity = function(numberofmeals,weather){
  
  electricitycooking <- sapply(numberofmeals,function(x){
    
    (abs(round(rnorm(1,mean=x*e_mean_factor,sd=e_sd))))^(e_curve)
  })
  # 10 kwh per degree lower then 20
  electricityheating <- sapply(weather, function(x){
    if(x < 20){
      (20-x)*10
    }else{0}
  })
  
  electricity <- round(electricitycooking+electricityheating,2)
  
  return (electricity)
}

#Description: Calculates the restaurant temperature
#It depends on number of customers and the outside temperatur
#Input: customers = vector of customer per hour, weather = outside temperature
#Output: Vector with water consumption data in the restaurant per hour.
get_restaurant_temperature = function(number_of_customers,outsidetemperature){
  #Scales number of customers and outside temperatur to interval [0,1]
  customer_scale = max(number_of_customers)-min(number_of_customers)
  scaled_customer = (number_of_customers-min(number_of_customers))/customer_scale
  outsidetemperature_scaler = max(outsidetemperature)-min(outsidetemperature)
  scaled_outsidetemperature = (outsidetemperature-min(outsidetemperature))/outsidetemperature_scaler
  
  #Density function is calculated
  restaurant_temperature_vector=((4/3)*scaled_outsidetemperature^3-scaled_customer+1)
  
  #Values are adjusted to fit to business case
  restaurant_temperature = restaurant_temperature_vector*2+20
  
  return(round(restaurant_temperature,1))
}

#Description: Calculates earned tips per hour
#The tips of course heavily depend on the number of customers. In addition the older the customers, the higher the average tip.
#Input: number_of_customers = vector of customer per hour, average_age = average age of customers per hour
#Output: Vector with water consumption data in the restaurant per hour.
get_tips = function(number_of_customers,average_age){
  averageAge[which(is.na(averageAge))] = 0
  tips = round((rnorm(length(number_of_customers),average_tip,variance_tip)+additional_tips_average_age*(average_age/max_age)),2)*number_of_customers 
}

#Description: calculates revenue per hour
#Revenues depend on consumed meals and drinks. Furthermore there is a light trend indicating that customers paying with cards tend to order more expensive meals than customers paying cash.
#Input: number_of_meals = vector of meals per hour, number_of_drinks = vector of drinks per hour, card_payments = = vector of card transactions per hour
#cash_payments = vector of cash transactions per hour
#Output: Vector with revenues of the restaurant per hour.
get_revenues = function(number_of_meals, number_of_drinks, card_payments,cash_payments){
  revenue_meals = rnorm(length(number_of_meals),average_price_meal,variance_meal_price)*number_of_meals
  revenue_drinks = rnorm(length(number_of_drinks),average_price_drink,variance_drink_price)*number_of_drinks
  
  #Check whether there were more card transactions than cash transactions
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

#Description: Create outliers for the entire data set
#There is a 1%-chance that it stays in data range and a 1%-chance that is doesnt stay in data range
#Input: Data set without outliers
#Output: Data set with outliers.
createoutliers <- function(dataset){
  
  for(k in 3:ncol(dataset)){
    
    minX <- min(dataset[k],na.rm=TRUE)
    maxX <- max(dataset[k],na.rm=TRUE)
    
    for (i in 1:length(dataset[,k])){
      
      if(!is.na(dataset[i,k])){
        random <- round(runif(1,1,1000))
        if(random == 999){

          randomoutlier = runif(1,minX,maxX)
          adaptedoutlier = trunc(randomoutlier*10^decimalnumcount(maxX))/10^decimalnumcount(maxX)
          dataset[i,k] = adaptedoutlier
        }
      }
    }
  }
  return(dataset)
}

#################### END FUNCTIONS ####################

#################### DATA INSTANCES ####################
#parameters for number of customers
mu1 <- 12  
mu2 <- 20
sig1 <- 1.5
sig2 <- 1.5
cpct <- 0.5 
n = floor(runif(1, 150, 250))
maxCapacity = 60
AnalyzedYear = 2000
startOfWork = 10
endOfWork = 24
minCustomerPerDay = 150
maxCustomerPerDay = 250

#parameters_gas
g_mean_factor = 500
g_sd = 1000
g_curve = 1/2

#parameters_water
w_mean_factor = 10
w_sd = 200
w_curve = 2/3

#parameters_elec
e_mean_factor = 100000
e_sd = 500000
e_curve = 1/3

#parameters_restaurant_temperature
fixed_temperature_for_cust = 18.0
fixed_temperature_for_opened_doors = 23.0
weight_factor_opened_doors = 0.55
x1 = 0.00001
x2 = 0.002
y1 = -0.0001

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

#################### END DATA INSTANCES ####################

####################   MAIN FUNCTION   ####################

create_dataset <- function(){
  print("Data sets are created...")
  times = getTimes()
  allcustomers = createAllCustomers(cpct, mu1, mu2, sig1, sig2)
  season = createSeason(times)
  season_nonnumeric = createSeason_nonnumeric(season)
  timeandcustomers = data.frame(times, allcustomers)
  averageAge = createAverageAge(allcustomers)
  tips=get_tips(timeandcustomers[,2],averageAge)
  doors_opened = createOpendoors(timeandcustomers[,2],season)
  outsidetemperature = createWeather()
  restaurant_temperature = get_restaurant_temperature(timeandcustomers[,2],outsidetemperature)
  mealsanddrinks = createMealsDrinks(timeandcustomers[,2])
  gas_consumption = createGas(mealsanddrinks[,1])
  water_consumption = createWater(mealsanddrinks[,1],outsidetemperature)
  electricity_consumption = createElectricity(mealsanddrinks[,1],outsidetemperature)
  paymentMethods = createPaymentMethods(timeandcustomers[,2],averageAge)
  revenues = get_revenues(mealsanddrinks[,1], mealsanddrinks[,2], paymentMethods[,1], paymentMethods[,2])

  dataset1 = data.frame("time" = times, "season" = season_nonnumeric, "customers" = allcustomers, "average_Age" = averageAge, 
                      "tips" = tips, "doors_opened" = doors_opened, "weather" = outsidetemperature, "restaurant_temperature" = restaurant_temperature, 
                      mealsanddrinks, "gas_consumption" = gas_consumption, "water_comsumption" = water_consumption, 
                      "electricity_consumption" = electricity_consumption, paymentMethods, revenues)

  dataset2 <- createoutliers(dataset1)
  print("Data sets created")
  return(dataset2)
}

final_dataset = create_dataset()

#################### END MAIN FUNCTION ####################