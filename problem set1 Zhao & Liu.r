library(readr)
Bluebikes <- read_csv("~/Desktop/2021fall term/6202Foundations of Data Analysis for Business/Problem set1/start-data.csv")
View(Bluebikes)

install.packages("mice")
library(mice) #visualize missing data

install.packages("dplyr")
library(dplyr)
library(ggplot2)

##Q1:
#Which variables, if any, appear to contain missing, null, or incorrect values?
 
#transform Date to character
Bluebikes$ride.date <- as.character(Bluebikes$ride.date) 
#<1> Let's find if our dataframe has missing values.
which(is.na(Bluebikes)) #Yes, there are NAs in our dataframe.
#to count the total number of NAs in dataframe
sum(is.na(Bluebikes)) #There are 16 NAs in our dataframe.
#to find NAs in columns
which(colSums(is.na(Bluebikes))!=0) #The column 14th,postal.code, has NAs.
#to find NAs in rows
which(rowSums(is.na(Bluebikes))!=0) #16 rows has NAs. 
#we find that the column postal.code contains all 16 missing values.
NA.Bluebikes<-Bluebikes[which(rowSums(is.na(Bluebikes))!=0),]
#visualize missing data, and the pink area is missing data. 
md.pattern(Bluebikes) 
#Let's find if our datafram has NULL values and incorrect value. 
table(Bluebikes$postal.code)
#We find there are 638 observations has NULL value in postal.code column. 
Null.Bluebikes <- Bluebikes[Bluebikes$postal.code == "NULL",  ]

#<2>The approach to handling these data errors. We replace NULL and NAs with "unknown". 
Bluebikes$postal.code <- replace(Bluebikes$postal.code, is.na(Bluebikes$postal.code), "unknown")
Bluebikes$postal.code <- replace(Bluebikes$postal.code, Bluebikes$postal.code == "NULL", "unknown")

##Q2:
##Using the z-score method (with z-score threshold of +/- 3), 
##what are the cutoff values for identifying outliers in the ‘tripduration’ variable? 

#<1> Let's create a new dataframe for "tripduration" named as Q2tripduration.
Q2tripduration <- data.frame(Bluebikes$tripduration)
#Calculate mean, standard deviation, and z-score
a <- mean(Q2tripduration$Bluebikes.tripduration) #17.11693
b <- sd(Q2tripduration$Bluebikes.tripduration)  #24.0711
Q2tripduration$Z.score <- (Q2tripduration$Bluebikes.tripduration - a)/b
#Compare each element's z-score with 3.
z1 <- 3
z2 <- -3
Q2tripduration$Outliers <- ifelse(Q2tripduration$Z.score > z1 | Q2tripduration$Z.score < z2,
                                   TRUE, FALSE)
#There are 87 outliers by comparing with z-score threshold of +/- 3. 
table(Q2tripduration$Outliers)
#Cutoff value 
upper.cutoff.value1 <- a + z1*b #89.33023
lower.cutoff.value2 <- a + z2*b #-55.09637   

#<2> Do you believe this method of identifying outliers is well-suited to the ‘tripduration’ data? Explain
#Let's plot density of trip duration so as to judge whether or not z-score is reliable. 
dens <- dnorm(Q2tripduration$Bluebikes.tripduration, mean=a, sd=b)
plot(Q2tripduration$Bluebikes.tripduration, dens, main="Trip Duration Distribution: Mean=17, SD:24",
     xlab="Trip Duration in Minutes", ylab="Probability Density", las=1)
abline(v=a)


##Q3:
##Using the boxplot/IQR method, what are the cutoff values for identifying outliers in the ‘tripduration’ variable?
#<1> draw the boxplot
boxplot(Bluebikes$tripduration, main="Boxplot for Trip Duration", xlab="Trip Duration in Minutes", 
        names="Bluebike", horizontal = TRUE, col ="gold")

#<2>identity the number of outliers
length(Bluebikes$tripduration) #7528 values in the column
median(Bluebikes$tripduration) #median = 12.01667
Q1 <- quantile(Bluebikes$tripduration, 0.25) #the Q1 = 7.466667
Q3 <- quantile(Bluebikes$tripduration, 0.75) #the Q3 = 20.1
IQR <- IQR(Bluebikes$tripduration) #IQR=12.63333
cutoff.value1 <- Q3+1.5*IQR #39.05
cutoff.value2 <- Q1-1.5*IQR #-11.48333
# there are 455 outliers by using IQR method
sum(ifelse(Bluebikes$tripduration > cutoff.value1 | Bluebikes$tripduration < cutoff.value2, 
           1, 0)) #455

##Q4: 
##create a dataframe with only rides that are up to 60 minutes in duration
Bluebikes.max60m <- Bluebikes[which(Bluebikes$tripduration<=60),]
#number of the rides
nrow(Bluebikes.max60m) #there are 7326 rides within 60 minutes. 

##Q5: 
##Create a histogram for the ‘tripduration’ variable.
hist(Bluebikes.max60m$tripduration, right=TRUE, main="Histogram of Trip duration within 60 Minutes",
     xlab="Riding Minutes", col = "Blue", ylim=c(0,2000))
#create a probability histogram for the ‘tripduration’ variable.
hist(Bluebikes.max60m$tripduration, freq=F, right=TRUE, main="Histogram of Trip duration within 60 Minutes",
     xlab="Riding Minutes", col = "Blue")
lines(density(Bluebikes.max60m$tripduration), col=2, lwd=3)

##Q6: 
##Create a contingency table and an accompanying stacked or clustered column chart to summarize 
##and visualize the variables ‘start.station.name’ and ‘usertype.’
#create contingency table
Contingency.table <- table(Bluebikes.max60m$start.station.name, Bluebikes.max60m$usertype)
Contingency.table
prop.table(Contingency.table)
#create stacked column chart
New.contingency <- table(Bluebikes.max60m$usertype,Bluebikes.max60m$start.station.name)
barplot(New.contingency,main = "Start Station Name VS User Type", 
        names.arg=c("Mass Ave","NEU","Ruggles","Tremont","Wentworth"), 
        col=c("blue", "red"),legend=rownames(New.contingency),
        xlab="Start Station Name", ylab = "User Number", 
        ylim=c(0,2500))
#Create clustered column chart
barplot(New.contingency, beside=TRUE, main = "Start Station Name VS User Type", 
        names.arg=c("Mass Ave","NEU","Ruggles","Tremont","Wentworth"), 
        col=c("blue", "red"),legend=rownames(New.contingency),
        xlab="Start Station Name", ylab = "User Number", 
        ylim=c(0,2000))

##Q7: 
##Create a table showing the average trip duration for rides originating at each of the start stations in the dataframe. 
#Method1
Q7 <- aggregate(tripduration~start.station.name, Bluebikes.max60m, mean)
#Method2
group.df1 <- Bluebikes.max60m %>%
  group_by(start.station.name) %>%
  summarise(ave.trip.duration = mean(tripduration), number.user = n())
write.csv(group.df1, file = "group_data1.csv", row.names = FALSE)# Export to butify the table

##Q8: 
##Create a table showing the average trip duration for rides taken by each ‘usertype’ in the dataframe (Customer and Subscriber).
#Method1
Q8 <- aggregate(tripduration~usertype, Bluebikes.max60m, mean)
#Method2
group.df2 <- Bluebikes.max60m %>%
  group_by(usertype) %>%
  summarise(ave.trip.duration = mean(tripduration), number.user = n())
write.csv(group.df2, file = "group_data2.csv", row.names = FALSE)# Export to butify the table


##Q9: 
##Suppose you randomly select a ride from the data set.
##What is the probability that the selected ride was taken by a Subscriber, as defined by the ‘usertype’ variable?
Total.people <- sum(table(Bluebikes.max60m$usertype)) #7326
Subscriber <- sum(Bluebikes.max60m$usertype=="Subscriber") #5301
Probability.subscriber <- Subscriber/Total.people  #0.7235872

##Q10: 
##Is the probability of selecting a Subscriber independent of the start station?
Contingency.table
sum(prop.table(Contingency.table)[,2]) #Unconditional probability=0.7235872
Conditinal.Probability1 <- Contingency.table[1,2]/(Contingency.table[1,2]+Contingency.table[1,1])#0.723435
Conditinal.Probability2 <- Contingency.table[2,2]/(Contingency.table[2,2]+Contingency.table[2,1])#0.7571166
Conditinal.Probability3 <- Contingency.table[3,2]/(Contingency.table[3,2]+Contingency.table[3,1])#0.7437468
Conditinal.Probability4 <- Contingency.table[4,2]/(Contingency.table[4,2]+Contingency.table[4,1])#0.7347866
Conditinal.Probability5 <- Contingency.table[5,2]/(Contingency.table[5,2]+Contingency.table[5,1])#0.5490196

##Q11: 
#calculated mean trip duration and proportion of users of type ‘Customer’ in each sample.
#<1>:What are the mean and standard deviation of the resulting sampling distribution of sample mean?
#sample mean of the trip duration
Population.mean <- mean(Bluebikes.max60m$tripduration) #14.65167
# So the mean of the resulting sampling distribution of sample mean is equal to 14.65166
#sample standard deviation of the trip duration
Population.sd <- sd(Bluebikes.max60m$tripduration) #10.11615
Sample.sd <- Population.sd/sqrt(50) # 1.430639

#<2>:What are the mean and standard deviation of the resulting sampling distribution of sample proportion?
#sample mean of proportion of users type ‘Customer'
prop.table(table(Bluebikes.max60m$usertype))
Proporation.mean <- 0.2764128
# so the mean of sample proporation is equal to 0.2764128
#sample standard deviation of the proportion of users type ‘Customer'
Proporation.sd <- sqrt((Proporation.mean*(1-Proporation.mean))/50) #0.06324694

##Q12: 
#Choose a random sample of 50 rides from the data and store these observations in a new dataframe called ‘sample.df.’
sample.df <- Bluebikes.max60m[sample(nrow(Bluebikes.max60m), 50), ]
View(sample.df)
t.test(sample.df$tripduration)
mean(Bluebikes.max60m$tripduration)

##Q13:
#Find the number of ‘Customer’ in “sample.df”
table(sample.df$usertype)
#We can see that there are 11 ‘Customer’
prop.test(11, 50, conf.level = 0.95)  #95 percent confidence interval: 0.1199448 0.3633110
#Calculate the mean of the true population proportion of ‘Customer’
sum(Bluebikes.max60m$usertype == "Customer") / 7326

#Q14
#(a)
#create the data from 2021-08-01 to 2021-08-07
week1 <- Bluebikes.max60m[which(Bluebikes.max60m$ride.date >= "2021-08-01" & Bluebikes.max60m$ride.date <= "2021-08-07"),]
mu0 <- mean(week1.Bluebikes.max60m$tripduration)
#Create the data of week4
week4 <- Bluebikes.max60m[which(Bluebikes.max60m$ride.date >= "2021-08-25" & Bluebikes.max60m$ride.date <= "2021-08-31"),]

#Create the dataframe of week4 with only tripduration and select the 100 sample
week4.tripduration <- data.frame(week4$tripduration)

set.seed(999)
sample.week4 <- week4.tripduration[sample(nrow(week4.tripduration), 100), ]
#test H0: x<=mu0  Ha: x >mu0
t.test(sample.week4, mu=mu0, alt="greater", conf=0.95)
#t = 1.8409, df = 99, p-value = 0.03432, 95 percent confidence interval: 14.75099 Inf


#(b)
sample.week4.withoutseed1 <- week4.tripduration[sample(nrow(week4.tripduration), 100), ]
sample.week4.withoutseed2 <- week4.tripduration[sample(nrow(week4.tripduration), 100), ]
sample.week4.withoutseed3 <- week4.tripduration[sample(nrow(week4.tripduration), 100), ]

m1 <- mean(sample.week4.withoutseed1)
m2 <- mean(sample.week4.withoutseed2)
m3 <- mean(sample.week4.withoutseed3)

s1 <- sd(sample.week4.withoutseed1)
s2 <- sd(sample.week4.withoutseed2)
s3 <- sd(sample.week4.withoutseed3)

#t = 1.3569, df = 99, p-value = 0.08895 > 0.05, fail to reject H0
t1 <- t.test(sample.week4.withoutseed1, mu=mu0, alt="greater", conf=0.95)
#t = 0.87406, df = 99, p-value = 0.1921 > 0.05, fail to reject H0
t2 <- t.test(sample.week4.withoutseed2, mu=mu0, alt="greater", conf=0.95)
#t = 0.785, df = 99, p-value = 0.2172 > 0.05, fail to reject H0
t3 <- t.test(sample.week4.withoutseed3, mu=mu0, alt="greater", conf=0.95)

August.sample.mean <- c(m1, m2, m3)
August.sample.sd <- c(s1, s2, s3)
August.sample.P <- c(t1$p.value, t2$p.value, t3$p.value)
August.sample.not.reject <- c(t1$p.value>0.05, t2$p.value>0.05, t3$p.value>0.05)
# Create a table for us to compare
August.df <- data.frame(August.sample.mean,August.sample.sd, August.sample.P,August.sample.not.reject)
write.csv(August.df, file = "August sample data.csv", row.names = FALSE)

##Q15
#<1>
tripduration123 <- data.frame(Bluebikes.max60m$tripduration)
set.seed(123)
sample123 <- tripduration123[sample(nrow(tripduration123), 100), ]
boxplot(sample123, main="Boxplot for Trip Duration of sample123 ", xlab="Trip Duration in Minutes", 
        names="Bluebike", horizontal = TRUE, col ="gold")

summary(sample123)
summary(Bluebikes$tripduration)

#<2>
#create a table showing the average trip duration for rides ending at each of the end stations in the dataframe.
group.df3 <- Bluebikes.max60m %>%
  group_by(end.station.name) %>%
  summarise(ave.trip.duration = mean(tripduration), number.user = n())
write.csv(group.df3, file="group3.csv", row.names =  FALSE)

#explore some potential linear regression between trip durations and number of users. 
fit1 <- lm(ave.trip.duration ~ number.user, data=group.df3)
#calculate t statistics
summary(fit1) 
ggplot(group.df3, aes(x=number.user, y=ave.trip.duration) ) +  
  geom_point(color="red", size=.5) +           
  geom_smooth(method="lm") +
  theme_classic()           








