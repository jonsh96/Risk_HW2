# B - Import data to R
test <- read.csv("C:\\Users\\Jón Sveinbjörn\\Desktop\\Áhættustýring\\Test50.csv")
training <- read.csv("C:\\Users\\Jón Sveinbjörn\\Desktop\\Áhættustýring\\Training50.csv")
myData <- read.csv("C:\\Users\\Jón Sveinbjörn\\Desktop\\Áhættustýring\\german_credit.csv")

summary(test)
summary(training)
summary(myData)

# C - Logistic Regression
myLogit <- glm(Creditability~., data = training, family = "binomial")
summary(myLogit)

# D - Choose variables into model (Sjá skýrslu)
# E - Measuring AIC values
myLogit1 <- update(myLogit,.~.-Occupation)
summary(myLogit1) 
myLogit2 <- update(myLogit1,.~.-Age..years.)
summary(myLogit2) 
myLogit3 <- update(myLogit2,.~.-Foreign.Worker)
summary(myLogit3) 
myLogit4 <- update(myLogit3,.~.-Most.valuable.available.asset)
summary(myLogit4) 
myLogit5 <- update(myLogit4,.~.-Concurrent.Credits)
summary(myLogit5) 
myLogit6 <- update(myLogit5,.~.-Duration.in.Current.address)
summary(myLogit6) 
myLogit7 <- update(myLogit6,.~.-Type.of.apartment)
summary(myLogit7) 
myLogit8 <- update(myLogit7,.~.-Duration.of.Credit..month.)
summary(myLogit8)
myLogit9 <- update(myLogit8,.~.-Telephone)
summary(myLogit9)
myLogit10 <- update(myLogit9,.~.-No.of.dependents)
summary(myLogit10)

myLogit11 <- update(myLogit10,.~.-(Intercept))
summary(myLogit11) 
myLogit12 <- update(myLogit11,.~.-No.of.Credits.at.this.Bank)
summary(myLogit12) 
myLogit13 <- update(myLogit11,.~.-Credit.Amount)
summary(myLogit13) 
myLogit14 <- update(myLogit11,.~.-Value.Savings.Stocks)
summary(myLogit14)
myLogit15 <- update(myLogit11,.~.-Instalment.per.cent)
summary(myLogit15)
myLogit16 <- update(myLogit11,.~.-Guarantors)
summary(myLogit16)
myLogit17 <- update(myLogit11,.~.-Length.of.current.employment)
summary(myLogit17)

min(drop1(myLogit11)$AIC) - myLogit11$aic

# F - Testing the quality of myLogit11 
install.packages("ROCR")
library(ROCR)
pred <- prediction(myLogit11$fitted, test$Creditability)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T, lwd= 3)
abline(a=0,b=1)
performance(pred, measure="auc")@y.values


# F - Testing the quality of myLogit
install.packages("ROCR")
library(ROCR)
pred <- prediction(myLogit$fitted, test$Creditability)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T, lwd= 3)
abline(a=0,b=1)
performance(pred, measure="auc")@y.values
