install.packages("chron");
library("chron");

getTimes <- function(){
  t <- seq(ISOdate(2000,1,1,hour = 10), ISOdate(2000,12,31, hour =24), "hours");
  t[hours(t)>=10]
}