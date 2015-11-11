require(data.table)
stores<-fread("store.csv")
train<-fread("train.csv")
test<-fread("test.csv")

#Add the variables from the stores
train <- merge(train, stores, by="Store")
test <- merge(test, stores, by="Store")
rm(stores)

#################################
# Data cleaning and preparation #
#################################

#Date variables
train$Date <- as.Date(train$Date)
test$Date <- as.Date(test$Date)

#Exclude closed shops
train <- train[which(train$Open==1),]

#54 shops in the train set are open but have no sales; exclude
train <- train[which(train$Sales>0),]

#Store 622 in the test dataset assumed to be open (shows NA)
test[which(test$Store=="622")]$Open<-1


##################################
# Naive model - first submission #
##################################

sales.avg <- mean(train[which(train$Open==1),]$Sales)
submit <- data.frame("Id"=test$Id, "Sales"=test$Open*sales.avg)
submit$Sales[is.na(submit$Sales)] <- sales.avg
write.csv(submit, "submission.csv", row.names=F)

#Demand model approach - second submission
fit.ols <- lm(Sales ~ DayOfWeek+Promo+Promo2+SchoolHoliday+
                    StoreType+Assortment+
                    CompetitionDistance, data=train)

summary(fit.ols)

submit$Sales <- 0
submit$Sales[which(test$Open==1)] <- predict(fit.ols, test[which(test$Open==1)])
submit$Sales[is.na(submit$Sales)] <- sales.avg
write.csv(submit, "submission.csv", row.names=F)

###########################################
#Demand model approach - third submission #
###########################################

fit.ols2 <- lm(log(Sales) ~ DayOfWeek+Promo+Promo2+SchoolHoliday+
                   StoreType+Assortment+
                   CompetitionDistance, data=train)
summary(fit.ols2)

#reset Sales
submit$Sales <- 0

#predict new values
submit$Sales[which(test$Open==1)] <- exp(predict(fit.ols2, test[which(test$Open==1)]))
submit$Sales[is.na(submit$Sales)] <- sales.avg
write.csv(submit, "submission.csv", row.names=F)

###########################################
#Demand model approach - fourth submission #
###########################################

require(plm)

panel <- pdata.frame(train, index = c("Store", "Date"),
                     drop.index = TRUE, row.names = TRUE)

fit.plm <- plm(Sales ~ DayOfWeek+Promo+Promo2+SchoolHoliday+
                  StoreType+Assortment+
                  CompetitionDistance, data=panel)
summary(fit.plm)

#reset Sales
submit$Sales <- 0

#predict new values
panel <- pdata.frame(test, index = c("Store", "Date"),
                     drop.index = TRUE, row.names = TRUE)
submit$Sales <- predict(fit.plm, panel)
submit$Sales[is.na(submit$Sales)] <- sales.avg
write.csv(submit, "submission.csv", row.names=F)
