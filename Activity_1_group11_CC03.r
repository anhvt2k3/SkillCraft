library(data.table)
library(GGally)
library(car)
data <- fread("D:\\R code\\house_price.csv") 
newData <- data[, c("price","sqft_living","floors","condition","sqft_above","sqft_living15")]
head(newData)

apply(is.na(newData),2, which)

apply(is.na(newData),2, sum)

newData$price[is.na(newData$price)] = mean(newData$price, na.rm=TRUE) 
apply(is.na(newData),2, which) #check again to make sure no NA values left

newData_2 <- newData

newData_2[, c("price","sqft_living","sqft_above","sqft_living15")]  <- log(newData_2[, c("price","sqft_living","sqft_above","sqft_living15")] + 1)

head(newData_2)

#DESCRIPTIVE STATISTIC BEFORE TAKING LOG(X + 1)
#mean
mean_1 <- apply(newData[, c("price","sqft_living","sqft_above","sqft_living15")], 2, mean)
#standard variance
sd_1 <- apply(newData[, c("price","sqft_living","sqft_above","sqft_living15")], 2, sd)
#min
min_1 <- apply(newData[, c("price","sqft_living","sqft_above","sqft_living15")], 2, min)
#max
max_1 <- apply(newData[, c("price","sqft_living","sqft_above","sqft_living15")], 2, max)
#median
median_1 <- apply(newData[, c("price","sqft_living","sqft_above","sqft_living15")], 2, median)
#plug in the table
data.frame(mean_1, sd_1, min_1, max_1, median_1)

#DESCRIPTIVE STATISTIC AFTER TAKING LOG(X + 1)
#mean
mean_2 <- apply(newData_2[, c("price","sqft_living","sqft_above","sqft_living15")], 2, mean)
#standard variance
sd_2 <- apply(newData_2[, c("price","sqft_living","sqft_above","sqft_living15")], 2, sd)
#min
min_2 <- apply(newData_2[, c("price","sqft_living","sqft_above","sqft_living15")], 2, min)
#max
max_2 <- apply(newData_2[, c("price","sqft_living","sqft_above","sqft_living15")], 2, max)
#median
median_2 <- apply(newData_2[, c("price","sqft_living","sqft_above","sqft_living15")], 2, median)
#plug in the table
data.frame(mean_2, sd_2, min_2, max_2, median_2)


hist(newData$price,
     main = paste("Histogram of house sale prices",
                  "for King County, 5/2014 - 5/2015"),
     xlab = "Price",
     col    = "thistle2",  # Color for histogram
     labels = TRUE,
     ylim=c(0,13000)
)

hist(newData_2$price,
     main = paste("Histogram of house sale prices",
                  "for King County, 5/2014 - 5/2015"),
     xlab = "log(price+1)",
     col    = "thistle2",  # Color for histogram
     labels = TRUE,
     ylim=c(0,8000)
     )

par(mfrow=c(1,2))
boxplot(price ~ floors,data = newData,
        main = "Boxplot of price for floors",
        col = c(2,3,4,5,6,7))

boxplot(price ~ floors, data = newData_2,
        main = "Boxplot of log(price+1) for floors",
        col = c(2,3,4,5,6,7))


par(mfrow=c(1,2))
boxplot(price ~ condition, data=newData,
        main = "Boxplot of price for condition",
        col = c(2,3,4,5,6,7))

boxplot(price ~ condition, data=newData_2,
        main = "Boxplot of log(price+1) for condition",
        col = c(2,3,4,5,6,7))


pairs(~newData$price + newData$sqft_living + newData$floors + newData$condition + newData$sqft_living15 +newData$sqft_above,
      data = newData,
      col = "steel blue",
      labels = c("price","sqft_living","floors","condition","sqft_living15","sqft_above"),
      main = "Pairs plot for price"
      )

pairs(~newData_2$price + newData_2$sqft_living + newData_2$floors + newData_2$condition + newData_2$sqft_living15 +newData_2$sqft_above,
      data = newData_2,
      col = "coral",
      labels = c("log(price+1)","log(sqft_living+1)","floors","condition","log(sqft_living15+1)","log(sqft_above+1)"),
      main = "Pairs plot for log(price+1)"
      )



#SPLIT DATA

sample <- sample(c(TRUE, FALSE), nrow(newData_2), replace=TRUE, prob=c(0.9,0.1))
train  <- newData_2[sample, ]
test   <- newData_2[!sample, ]



lm_model1 <- lm(price ~  sqft_living + floors + condition + sqft_above + sqft_living15,
                 data = train)
summary(lm_model1)
vif(lm_model1)


lm_model2 <- lm(price ~ sqft_living + floors + condition + sqft_living15 ,
                data = train)

summary(lm_model2)
vif(lm_model2)

#residual and fitted
plot(lm_model2,
     col = "steel blue",
     which = 1)

#Normal Q-Q
plot(lm_model2,
     col = "steel blue",
     which = 2)

#Scale - Location
plot(lm_model2,
     col = "steel blue",
     which = 3)

#Cook's distance
plot(lm_model2,
     col = "steel blue",
     which = 4)

#residuals vs leverage
plot(lm_model2,
     col = "steel blue",
     which = 5)

predicts <- predict(lm_model2, newdata = test)
actuals <- test$price
evaluate <- data.frame(actuals,predicts)
summary(evaluate)

m <- lm(actuals~predicts,data = evaluate)
summary(m)





