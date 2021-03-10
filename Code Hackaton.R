setwd("/Users/gorkeoceane/Desktop/hackathon/")
data_train <- read.csv('data.csv')
test <- read.csv('test.csv')
dim(data_train)
str(data_train)
data <- na.omit(data_train) 
sum(is.na(data))
head(data)
library(randomForest)
#
myglm<- glm(SeriousDlqin2yrs ~ .,family=binomial(link='logit') , data = data)
summary(myglm)
pred<- predict(myglm, newdata=test,type = "response")
pred

y <- ifelse(pred>0.5, "1", "0")
y


#
library(rpart)
library(rpart.plot)

tree1 <- rpart(SeriousDlqin2yrs ~., data=data)
summary(tree1)
plot(tree1)
text(tree1)

printcp(tree1) 

#method prune
#min for indice = 5
#min = 0.84480 + 0.012076
# = 0.856876
#cp = 0.014983

plotcp(tree1)

tree1_prune <- prune(tree1,cp=  0.026631)
rpart.plot(tree1_prune)

tree1_pred = predict(tree1, newdata=test)
tree1_pred

y <- ifelse(tree1_pred>0.5, "1", "0")
y
#same score : 0,94230


#
randomtree <- randomForest(SeriousDlqin2yrs ~ ., data = data, n.trees=500, mtry = ncol(data)-1)
  
summary(randomtree)

#
library(gbm)
model_boost <- gbm(SeriousDlqin2yrs~ .,data=data, distribution="bernoulli",interaction.depth=1,shrinkage=0.01)
#tried to change interaction.depth (1, 2, 3), shrinkage
plot(model_boost)
pred <- predict(model_boost, newdata=test)
pred
y <- ifelse(pred>0.5, "1", "0")
y

#
library(randomForest)
model_Bagging = randomForest(as.factor(SeriousDlqin2yrs) ~ ., data = data, mtry = sqrt(10),
                             importance = TRUE)

pred <- predict(model_Bagging, newdata=test)
pred
#no need to do ifelse on this one, it was directly between 0 and 1s

y <- ifelse(pred>0.5, "1", "0")
y

#
fit <- randomForest(as.factor(SeriousDlqin2yrs) ~ . -MonthlyIncome,
                    data=data, 
                    importance=TRUE, 
                    ntree=25, keep.forest = TRUE)
sum(is.na(data$SeriousDlqin2yrs))

pred <- predict(fit, test)
pred

#

to_be_submitted = data.frame(id=rownames(test), SeriousDlqin2yrs=pred)
write.csv(to_be_submitted , file = "to_be_submitted.csv", row.names = F)