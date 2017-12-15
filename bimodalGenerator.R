mu1 <- 12  
mu2 <- 18
sig1 <- 1
sig2 <- 1.5
cpct <- 0.5   

bimodalDistFunc <- function (n,cpct, mu1, mu2, sig1, sig2) {
  y0 <- rnorm(n,mean=mu1, sd = sig1)
  y1 <- rnorm(n,mean=mu2, sd = sig2)
  
  flag <- rbinom(n,size=1,prob=cpct)
  y <- y0*(1 - flag) + y1*flag 
}

bimodalData <- floor(bimodalDistFunc(n=200,cpct,mu1,mu2, sig1,sig2))


bimodaldataTest <- floor(bimodalData)


sortbimodaldata = function(x){
  temp = x
  if(max(x) > 23){
    temp = temp[-which(temp>23)]
  } else if(min(temp)<10){
    temp = temp[-which(temp<10)]
  }
  return(temp)
}


testrun = function(){
  data = bimodalDistFunc(200, 0.5, 12, 18, 1,1.5)
  floordata = floor(data)
  sorteddata = sortbimodaldata(floordata)
  return(sorteddata)
}

test = testrun()
hist(test)

length(test[which(test==10)])
