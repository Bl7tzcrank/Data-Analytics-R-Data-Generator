install.packages("chron");
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

#####################################################end of functions###########################################

#####################################################begin of data instances###########################################
mu1 <- 12  
mu2 <- 18
sig1 <- 1
sig2 <- 1.5
cpct <- 0.5 
n = floor(runif(1, 150, 250))
#####################################################end of data instances###########################################

#####################################################begin of main part###########################################

allcustomers = createAllCustomers(cpct, mu1, mu2, sig1, sig2)
length(allcustomers)

times = getTimes()

timeandcustomers = data.frame(times, allcustomers)


max(timeandcustomers$allcustomers)

