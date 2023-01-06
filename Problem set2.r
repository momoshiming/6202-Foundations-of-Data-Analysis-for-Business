library(readr)
commutervan <- read_csv("~/Desktop/Problem set2/commutervan.csv")
View(commutervan)

options(scipen = 999)
######
##Q1
lm1 <- lm(rides ~ booked.ride, data = commutervan)
summary(lm1)

######
##Q2
#Regress all possible variables
multi.lm1 <- lm(booked.ride ~ starts.session + tapped.sidebar + tapped.on.stop + viewed.eta,
               data= commutervan)
summary(multi.lm1)

#Regress three predictor variables omitting tapped.on.stop
multi.lm2 <- lm(booked.ride ~ starts.session + tapped.sidebar + viewed.eta,
                data= commutervan)
summary(multi.lm2)

#Regress two predictor variables omitting tapped.on.stop and tapped.sidebar
multi.lm3 <- lm(booked.ride ~ starts.session + viewed.eta,
                data= commutervan)
summary(multi.lm3)

######
##Q3
lm.Q3 <- lm(rides ~ t, data = commutervan)
plot(rides ~ t, main="Number of Rides Completed from 1st to 64th Day", 
     ylab="Number of Rides", xlab="Days", 
     col="lightblue", pch=19, cex=1, data = commutervan, type="b")
text(rides ~ t, labels=t,data=commutervan, cex=0.5, font=2)
abline(lm.Q3, col="red", lwd=2)

######
##Q4
Q4.commutervan <- data.frame(commutervan$t, commutervan$rides)
Q4.commutervan$rides.hat <- NA

#replace rides' zero value with nearby value
a <- which (Q4.commutervan$commutervan.rides == 0)  
replace.a <- a + 1
Q4.commutervan$commutervan.rides[a] <- Q4.commutervan$commutervan.rides[replace.a]

#prediction of rides using SMA
for (i in 1:59) {
  b = i + 4
  Q4.commutervan$rides.hat[i+5] <- mean(Q4.commutervan$commutervan.rides[i:b])
}
#Errors
Q4.commutervan$errors <- Q4.commutervan$commutervan.rides - Q4.commutervan$rides.hat
#Errors^2
Q4.commutervan$errors.square <- Q4.commutervan$errors^2
#Abs.Error
Q4.commutervan$abs.error <- abs(Q4.commutervan$errors)
#Abs%error
Q4.commutervan$abs.per.error <- abs(Q4.commutervan$errors/Q4.commutervan$commutervan.rides)

#MSE MAD MAPE
Q4.MSE <- mean(Q4.commutervan$errors.square[6:64])
Q4.MAD <- mean(Q4.commutervan$abs.error[6:64])
Q4.MAPE <- mean(Q4.commutervan$abs.per.error[6:64])

######
##Q5
lm.Q5 <- lm(commutervan.rides ~ commutervan.t, data=Q4.commutervan)
summary(lm.Q5)

######
##Q6
#create new datafram that concludes only rides, time, and day of week
Q6.commutervan <- Q4.commutervan[1:2]
Q6.commutervan$dayofweek <- commutervan$dayofweek

#create day-of-week dummy variables for the ‘rides’ variable
Q6.commutervan$d1 <- ifelse(Q6.commutervan$dayofweek == "Monday", 1, 0)
Q6.commutervan$d2 <- ifelse(Q6.commutervan$dayofweek == "Tuesday", 1, 0)
Q6.commutervan$d3 <- ifelse(Q6.commutervan$dayofweek == "Wednesday", 1, 0)
Q6.commutervan$d4 <- ifelse(Q6.commutervan$dayofweek == "Thursday", 1, 0)

#create regression model with 5 variables
lm.Q6 <- lm(commutervan.rides ~ d1 + d2 + d3 + d4 + commutervan.t, data = Q6.commutervan)
summary(lm.Q6)

######
##Q7

###(a)
#prediction of everyday rides using Q6 regression model
Q6.commutervan$rides.hat <- predict.lm(lm.Q6, Q6.commutervan)

#Errors
Q6.commutervan$errors <- Q6.commutervan$commutervan.rides - Q6.commutervan$rides.hat
#Errors^2
Q6.commutervan$errors.square <- Q6.commutervan$errors^2
#Abs.Error
Q6.commutervan$abs.error <- abs(Q6.commutervan$errors)
#Abs%error
Q6.commutervan$abs.per.error <- abs(Q6.commutervan$errors/Q6.commutervan$commutervan.rides)

#MSE MAD MAPE
Q7.MSE <- mean(Q6.commutervan$errors.square)
Q7.MAD <- mean(Q6.commutervan$abs.error)
Q7.MAPE <- mean(Q6.commutervan$abs.per.error)

###(b)
#predict daily completed rides for each weekday in the next month (April 1-April 29)
write.csv(Q6.commutervan, file="Prediction.csv") # we use excel to make prediction
# then we import the result into the R
library(readr)
Prediction <- read_csv("~/Desktop/Prediction.csv")
View(Prediction)  



