setwd("C:/Users/kos_c/Desktop/Statistics II Assignment I/Karlis Main I")
load("C:/Users/kos_c/Desktop/Statistics II Assignment I/Karlis Main I/.RData")
myopia <- read.csv2("myopia.csv")
myopia<-myopia[,c(-1,-2)]
str(myopia)
summary(myopia)

myopia$GENDER <- factor(myopia$GENDER)
myopia$MOMMY <- factor(myopia$MOMMY)
myopia$DADMY<- factor(myopia$DADMY)

library(sjPlot)
sjt.df(myopia)


barplot(table(myopia$GENDER),col = "lightgreen",ylim = c(0,350),xlab = "Gender",ylab = "Myopic",main = "Myopic per Gender")
barplot(table(myopia$MOMMY),col = "pink",ylim = c(0,350),xlab = "Mother",ylab = "Myopic",main = "Myopic Mother")
barplot(table(myopia$DADMY),col = "lightblue",ylim = c(0,350),xlab = "Father",ylab = "Myopic",main = "Myopic Father")


sjt.frq(myopia$MYOPIC, variableLabels=list("Myopic"), autoGroupAt=10)

sjt.corr(myopia[,c(-1,-3,-15,-16)])


library(corrplot)
newdatacor = cor(myopia[,c(-1,-3,-15,-16)])
corrplot(newdatacor, method = "number")
myopia<-myopia[,-5]

# Build Unregularized Model
mylogit = glm(MYOPIC ~ ., family = binomial(link = "logit"), data=myopia)
summary(mylogit)

#Partition
library(caret)
Train <- createDataPartition(myopia$MYOPIC, p=0.8, list=FALSE)
training <- myopia[ Train, ]
testing <- myopia[ -Train, ]

#Stepwise
step<-step(mylogit)
summary(step)

# Lasso with glmnet
library(glmnet)
myo_mat <- model.matrix(MYOPIC~., training)
lambdas <- 10 ^ seq(8,-4,length=250)
myo_models_lasso <- glmnet(myo_mat,training$MYOPIC,alpha=1, lambda=lambdas, family="binomial")

#plots
plot(myo_models_lasso)
grid()
plot(myo_models_lasso, xvar = "dev", label = TRUE)
#plot variable coefficients vs. shrinkage parameter lambda.
plot(myo_models_lasso, xvar = "lambda", label = TRUE)

# Lasso Models
print(myo_models_lasso)

# Extract Coefficients
coef(myo_models_lasso, s = c(1,0.1,0.01,0.001))
logit = glm(MYOPIC ~ GENDER + SPHEQ + ACD + VCD + SPORTHR + READHR + STUDYHR + MOMMY + DADMY , family = binomial(link = "logit"), data=training)


# Cross Validation
lasso.cv <- cv.glmnet(myo_mat,training$MYOPIC, alpha=1, lambda=lambdas, family="binomial")

# Alternative Measures of Performance
lasso.cv2 <- cv.glmnet(myo_mat,training$MYOPIC,alpha=1,lambda=lambdas, family="binomial", type.measure = "class")
lasso.cv3 <- cv.glmnet(myo_mat,training$MYOPIC,alpha=1,lambda=lambdas, family="binomial", type.measure = "auc")
lasso.cv4 <- cv.glmnet(myo_mat,training$MYOPIC,alpha=1,lambda=lambdas, family="binomial", type.measure = "mse")
lasso.cv5 <- cv.glmnet(myo_mat,training$MYOPIC,alpha=1,lambda=lambdas, family="binomial", type.measure = "mae")

# Cross Validation Plots
plot(lasso.cv)
plot(lasso.cv2)
lasso.cv$lambda.min
lasso.cv$lambda.1se

# Many Facets of predict()
predict(myo_models_lasso, type="coefficients", s = lasso.cv$lambda.min)
preds <- predict(myo_models_lasso, myo_mat, type = "response", s = c(lasso.cv$lambda.min, lasso.cv$lambda.1se))
head(preds)
preds <- predict(myo_models_lasso, myo_mat, type = "class", s = lasso.cv$lambda.min)

# Performance
table(predicted = preds, actual = training$MYOPIC)
mean(preds  == training$MYOPIC)

#Checking for independence.
library(randtests)
runs.test(step$residuals)
library(lmtest)
dwtest(step)
library(car)
durbinWatsonTest(step)
dwt(step)
dwt(step$residuals)