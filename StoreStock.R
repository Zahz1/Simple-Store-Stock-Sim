lambda_a <- 6
lambda_d <- 1
r <- 1.3
cost <- function(y){20 + y}
s <- 180
S <- 350
L <- 24
H <- 0.01

t <- 0
profit <- 0
stock <- S
order <- 0
ordersLostCount <- 0
day <- 0

tArrival <- rexp(1,lambda_a)
tDelivery <- Inf

mat <- c(t,tArrival,tDelivery,profit,stock,order, ordersLostCount, day)

updateStep <- function() {
  if (stock < s && order == 0){
    #order event
    tDelivery = 24
    order = S - stock
    orderPrice <- cost(order)
    profit <- profit - orderPrice
    
  } else {
    #customer event
    nextT <- t + tArrival;
    
    if ( nextT/24 >= day + 1 ) {
      inventoryPay <- stock * H
      profit <- profit - inventoryPay
      day = day + 1
    }
    
    t = nextT
    
    if (order != 0) {
      #decrement order delivery time
      if (tDelivery - tArrival <= 0) {
        #restock b/c order has arrived!
        tDelivery = Inf
        stock <- stock + order
        order = 0
      } else {
        #delivery has not arived, decrement timer for arrival 
        tDelivery <- tDelivery - tArrival
      } 
    }
    amountBuying <- rpois(1, lambda_d)
    if (stock >= amountBuying) {
      amountPaying <- amountBuying*r
      profit <- profit + amountPaying
      stock <- stock - amountBuying
    } else {
      ordersLostCount <- ordersLostCount + 1
    }
    tArrival <- rexp(1,lambda_a)
  }
  
  
  mat <- rbind( mat, c(t,tArrival,tDelivery,profit,stock,order, ordersLostCount, day) )
  mat
}