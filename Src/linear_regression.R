house.data = readRDS("../Data/housing_data.rds")

#find skewness
find_skewness(house.data, index = FALSE)
#compute skweness 
find_skewness(house.data, value = TRUE)
#transform totalprice
house.data$totalPriceLog = transform(house.data$totalPrice, method = "log+1")
#transform ladderRatio
house.data$ladderRatioLog = transform(house.data$ladderRatio, method = "log+1")

#analysis: between 2014 - 2017
house.data$tradeTimeTs <- as.Date(house.data$tradeTime, format = "%d/%m/%Y")
house.data$year <- year(house.data$tradeTimeTs)
house.train <- data.frame(house.data %>% filter(year>2009 & year<2017))
house.test <- data.frame(house.data %>% filter(year>2016))
set.seed(1)

#decided to not cv
#k-fold: define train control
#train.control <- trainControl(method="repeatedcv", number = 10, repeats = 3)
#train.control <- trainControl(method = "cv", number = 10)

#peek
model.all = lm(totalPriceLog ~ ., data = house.train)

#training
#with rooms
to.exclude = c(1:5, 15, 26, 27, 7, 18)
#without rooms
to.exclude = c(1:5, 9:12, 15, 26, 27, 7, 18)

house.trained = house.train[,-to.excluded.col]
model.trained = train(totalPriceLog ~., data = house.trained, method = "lm", trControl = train.control)

summary(k.model)
print(k.model)

#prediction 
prediction <- predict(model.trained, newdata = house.test)

# if u need to check
# sse = sum((house.test$totalPriceLog - prediction) ^2)
# sst = sum((house.test$totalPriceLog - mean(house.test$totalPriceLog))^2)
# rsquared
# 1 - (sse / sst)
# degFreedom = nrow(house.test) - 14
# mse
# sse / degFreedom
