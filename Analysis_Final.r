#RUN LAST

library(dplyr)
library(reshape2)

# Creating incarceration rate percentage column and adding to 'all'
all$total_incarcerated<-all$violent_crime_total+all$property_crime_total

all$incarceration_rate<-(all$total_incarcerated/all$state_population)*100
View(all)

#ANALYSIS PART 1 - SUMMARY TABLES

#Highest/Lowest Murder

all$Murder_per_capita <- (all$murder_manslaughter/all$state_population) * 100000

df1 <- data.frame(all$jurisdiction1, all$Murder_per_capita, all$Year1)
Murder_Ordered <- df1[order(df1$all.Murder_per_capita),]

View(Murder_Ordered)

#Highest Violent Crimes

all$Violent_per_capita <- (all$violent_crime_total/all$state_population) * 100000

df2 <- data.frame(all$jurisdiction1, all$Violent_per_capita, all$Year1)
Violent_Ordered <- df2[order(df2$all.Violent_per_capita),]

View(Violent_Ordered)

#Highest Property Crimes

all$Property_per_capita <- (all$property_crime_total/all$state_population) * 100000

df3 <- data.frame(all$jurisdiction1, all$Property_per_capita, all$Year1)
Property_Ordered <- df3[order(df3$all.Property_per_capita),]

View(Property_Ordered)

#Change across States

State_changes <- subset(all, (jurisdiction1 == "NewMexico" | jurisdiction1 == "NewHampshire" | jurisdiction1 == "Alaska" | jurisdiction1 == "Vermont" | 
                                jurisdiction1 == "Louisiana"),select = c("jurisdiction1", "Classification1","Year1"))



#ANALYSIS PART 2 - CHI2, CORRELATION, REGRESSION

# running chi-sq test on incarceration rate vs democractic advantage
incarceration_test<-chisq.test(table(all$incarceration_rate,all$Democratic_Advantage1))
incarceration_test

# changing democratic advantage to numeric
str(all)
all$Democratic_Advantage1<-as.numeric(all$Democratic_Advantage1)

# Hypothesis test for significant correlation between democratic advantafe and incerceration rate
ct<-cor.test(all$incarceration_rate,all$Democratic_Advantage1)
class(ct)
ct$p.value

# linear regression on the incarceration rate and democractic advantage
fit<-lm(incarceration_rate~Democratic_Advantage1, data=all)
summary(fit)


## Check Assumptions ##
# Linearity: correlation
cor(all$incarceration_rate,all$Democratic_Advantage1)

# Visualize relationship with scatter plot:
plot(incarceration_rate~Democratic_Advantage1, data=all)
# Add regression line with abline()
abline(fit, col="red")

# Normality: Q-Q plot of residuals
qqnorm(fit$residuals)
qqline(fit$residuals, col="red")

# Homoscedasticity: plot fitted values vs. residuals
plot(fit$fitted.values, fit$residuals,
     xlab="Fitted Values", ylab="Residuals")

#ANALYSIS PART 3 - ANNOVA

#Analysis of Variance
#Is there any statistical differences between the means of three or more independent groups?

#Create new column to make 3 classifications 1-Rep 2-Dem 3-Competitive
View(all)
#write.csv(all, "all.csv")
all$Classification2 <- ifelse(all$Classification1 == "Solid Rep", "Rep",
                              ifelse(all$Classification1 == "Lean Rep", "Rep",
                              ifelse(all$Classification1 == "Solid Dem", "Dem",
                              ifelse(all$Classification1 == "Lean Dem", "Dem",
                              ifelse(all$Classification1 == "Competitive", "Competitive",
                              ifelse(all$Classification1 == "Solid Rep", "Rep",
                              ifelse(all$Classification1 == "Lean Rep", "Rep",
                              ifelse(all$Classification1 == "Solid Dem", "Dem", NA))))))))

# all Incarceration Rate means are equal across Democrat, Republican, or Competitive states
aov1 <- aov(incarceration_rate~Classification2,
            data = all)
summary(aov1)
# P-value < 0.05 (0.012)
# We reject the null hypothesis that the Incarceration Rate means are equal for Dem, Rep, or Competitive states
# Unequal variances
TukeyHSD(aov1)
# high p value means it isn't very significant
# statistical difference between groups if the p value for two categories compared is less than 0.05

# there is a statistical difference in means between Republican and Democrat in incarceration rate because the
# p val is less 0.05 (0.0147673)

#ANALYSIS PART 4 - REGRESSION

all$Republican_Lean1 <- as.numeric(all$Republican_Lean1)
all$Democrat_Lean1 <- as.numeric(all$Democrat_Lean1)
all$Democratic_Advantage1 <- as.numeric(all$Democratic_Advantage1)
all$incarceration_rate <- (all$violent_crime_total+all$property_crime_total)/all$state_population*100

########## Linear Regression

########## Is the density of violent crimes in each state related to the democratic and republican
head(all)

plot(all$Democrat_Lean1,all$incarceration_rate)
cor.test(all$Democrat_Lean1,all$incarceration_rate)

plot(all$Republican_Lean1,all$incarceration_rate)
cor.test(all$Republican_Lean1,all$incarceration_rate)

plot(all$Democratic_Advantage1,all$incarceration_rate)
cor.test(all$Democratic_Advantage1,all$incarceration_rate)

fit <- lm(incarceration_rate~Democratic_Advantage1, data=all)
summary(fit)


#ANALYSIS 5 - Creating histogram for each year  

hist_2017 <- barplot(table(Party1$Classification1), ylab= "Frequency", las = 2, main = "2017", col="lightcoral" )
hist_2016 <- barplot(table(Party2$Classification1), ylab= "Frequency", las = 2, main = "2016", col = "lightcoral" )
hist_2015 <- barplot(table(Other$Classification1), ylab= "Frequency", las = 2, main = "2015", col = "lightcoral" )

#remove DC and US 
Party1 <- Party1[-c(1, 52),]
Party_All <- Party_All[-c(1, 52),]

#Line graph- lines show number of crimes, how the types of crimes increase/decrease over year
library(ggplot2)
library(data.table)

Violent <- c(sum(all$violent_crime_total[all$Year1 == "2015"]),sum(all$violent_crime_total[all$Year1 == "2016"]),sum(all$violent_crime_total[all$Year1 == "2017"]))
Violent
Property <- c(sum(all$property_crime_total[all$Year1 == "2015"]),sum(all$property_crime_total[all$Year1 == "2016"]),sum(all$property_crime_total[all$Year1 == "2017"]))
Property
Years <- c("2015", "2016", "2017")
Years

Linedf <- data.frame(Violent, Property, Years)

plot(Linedf$Violent)
lines(Linedf$Property)

plot(Linedf$Property,type="b",col="lightblue",lty=1,ylab="Number of Crimes",lwd=2, ylim = c(0, 8000000),xlab="Year",xaxt="n", main = "Property Crimes vs. Violent Crimes by Year", cex = 2)
lines(Linedf$Violent,type="b",col="lightcoral",lty=1,lwd=2, cex = 2,)
axis(1,at=c(1:3),labels= Years)
legend("bottomright", legend = c("Property Crimes", "Violent Crimes"),
       lwd = 3, col = c("lightblue", "lightcoral"))
text(Linedf,
     labels = row.names(Linedf$Years),
     cex = 0.6, pos = 4, col = "black")
















