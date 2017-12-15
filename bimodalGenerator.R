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



mu1 <- 12  
mu2 <- 18
sig1 <- 1
sig2 <- 1.5
cpct <- 0.5 

test = createCustomerPerDay(n,cpct, mu1, mu2, sig1, sig2)

