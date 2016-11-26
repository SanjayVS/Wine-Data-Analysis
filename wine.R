rm(list = ls())

setwd("C:/Users/Sanjay/Desktop/Wine Case Study")

library("ggplot2")
library("psych")
#Reading csv data 
red_wine = read.csv("winequality-red.csv", header = TRUE, sep = ";")
white_wine = read.csv("winequality-white.csv", header = TRUE, sep = ";")
red_wine$type =  "Red"
white_wine$type = "White"
red_wine$type = as.factor(red_wine$type)
white_wine$type = as.factor(white_wine$type)

#Removing duplicate Entries.Found many duplicate values in the dataset
#which will cause problem. Eg:1995 and 2005 in white wine data
white_wine = unique(white_wine)
red_wine= unique(red_wine)

#Rearranging columns to make quality last
red_wine = red_wine[,c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide",
               "total.sulfur.dioxide","density","pH","sulphates","alcohol","type","quality")]
white_wine = white_wine[,c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide",
               "total.sulfur.dioxide","density","pH","sulphates","alcohol","type","quality")]

#merging the data to create master
wine = rbind(red_wine, white_wine)


#Checking for missing values
apply(wine,2, function(x)sum(is.na(x)))


#Creating separate histograms to analyze white and red wine data. As there are 30 bins by default, taking binwidth as Range/30

range = max(red_wine$pH) - min(red_wine$pH)
ggplot(red_wine , aes_string(x=red_wine$pH))+
  geom_histogram(fill = "GoldenRod", stat="bin" , binwidth = range/30, colour="red") + theme_bw() + xlab("pH") +
  ylab("Frequency") + ggtitle("pH Distribution") + theme(text = element_text(size = 10))  

range = max(red_wine$alcohol) - min(red_wine$alcohol)
ggplot(red_wine , aes_string(x=red_wine$alcohol))+
  geom_histogram(fill = "GoldenRod", stat="bin", binwidth= range/30,colour="red") + theme_bw() + xlab("Quality") +
  ylab("Frequency") + ggtitle("Alcohol Distribution") + theme(text = element_text(size = 10))  

range = max(red_wine$volatile.acidity) - min(red_wine$volatile.acidity)
ggplot(red_wine , aes_string(x = red_wine$volatile.acidity)) +
  geom_histogram(fill = "GoldenRod", stat="bin" , binwidth = range/30, colour="red") + theme_bw() + xlab("Volatile acidity") +
  ylab("Frequency") + ggtitle("Volatile Acidity Distribution") + theme(text = element_text(size = 10))  

range = max(red_wine$free.sulfur.dioxide) - min(red_wine$free.sulfur.dioxide)
ggplot(red_wine , aes_string(x = red_wine$free.sulfur.dioxide)) +
  geom_histogram(fill = "GoldenRod", stat="bin" ,  binwidth = range/30, colour="red") + theme_bw() + xlab("Free SO2") +
  ylab("Frequency") + ggtitle("Free SO2 distribution") + theme(text = element_text(size = 10))  

range = max(white_wine$pH) - min(white_wine$pH)
ggplot(white_wine , aes_string(x=white_wine$pH))+
  geom_histogram(fill = "GoldenRod", stat="bin" , binwidth = range/30, colour="red") + theme_bw() + xlab("pH") +
  ylab("Frequency") + ggtitle("pH Distribution") + theme(text = element_text(size = 10))  

range = max(white_wine$alcohol) - min(white_wine$alcohol)
ggplot(white_wine , aes_string(x=white_wine$alcohol))+
  geom_histogram(fill = "GoldenRod", stat="bin", binwidth= range/30,colour="red") + theme_bw() + xlab("Quality") +
  ylab("Frequency") + ggtitle("Alcohol Distribution") + theme(text = element_text(size = 10))  

range = max(white_wine$volatile.acidity) - min(white_wine$volatile.acidity)
ggplot(white_wine , aes_string(x = white_wine$volatile.acidity)) +
  geom_histogram(fill = "GoldenRod", stat="bin" , binwidth = range/30, colour="red") + theme_bw() + xlab("Volatile acidity") +
  ylab("Frequency") + ggtitle("Volatile Acidity Distribution") + theme(text = element_text(size = 10))  

range = max(white_wine$free.sulfur.dioxide) - min(white_wine$free.sulfur.dioxide)
ggplot(white_wine , aes_string(x = white_wine$free.sulfur.dioxide)) +
  geom_histogram(fill = "GoldenRod", stat="bin" ,  binwidth = range/30, colour="red") + theme_bw() + xlab("Free SO2") +
  ylab("Frequency") + ggtitle("Free SO2 distribution") + theme(text = element_text(size = 10))  

#Checking distribution of the variables by quality
tapply(wine$alcohol, wine$quality, mean)
tapply(wine$citric.acid, wine$quality, mean)
tapply(wine$pH, wine$quality, mean)
tapply(wine$volatile.acidity, wine$quality, mean)
tapply(wine$free.sulfur.dioxide, wine$quality, mean)
tapply(wine$fixed.acidity, wine$quality, mean)

summary(wine)

#Plotting out the outliers using boxplot

par(mfrow=c(1,5), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)

boxplot(wine$fixed.acidity, col="slategray2", pch=19)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)
boxplot(wine$volatile.acidity, col="slategray2", pch=19)
mtext("Volatile Acidity", cex=0.8, side=1, line=2)
boxplot(wine$citric.acid, col="slategray2", pch=19)
mtext("Citric Acid", cex=0.8, side=1, line=2)
boxplot(wine$residual.sugar, col="slategray2", pch=19)
mtext("Residual Sugar", cex=0.8, side=1, line=2)
boxplot(wine$chlorides, col="slategray2", pch=19)
mtext("Chlorides", cex=0.8, side=1, line=2)

par(mfrow=c(1,5), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
boxplot(wine$alcohol, col="slategray2", pch=19)
mtext("Alcohol", cex=0.8, side=1, line=2)
boxplot(wine$pH, col="slategray2", pch=19)
mtext("pH", cex=0.8, side=1, line=2)
boxplot(wine$sulphates, col="slategray2", pch=19)
mtext("Sulphates", cex=0.8, side=1, line=2)
boxplot(wine$total.sulfur.dioxide, col="slategray2", pch=19)
mtext("Total SO2", cex=0.8, side=1, line=2)
boxplot(wine$free.sulfur.dioxide, col="slategray2", pch=19)
mtext("Free SO2", cex=0.8, side=1, line=2)

par(mfrow=c(1,5), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
boxplot(wine$density, col="slategray2", pch=19)
mtext("Density", cex=0.8, side=1, line=2)

#Removing the outliers with quality values 5 and 6
data1 = wine[which((wine$quality == 3)),]
data1 = rbind(data1 , wine[which((wine$quality == 4)),])
data1 = rbind(data1 , wine[which((wine$quality == 7)),])
data1 = rbind(data1 , wine[which((wine$quality == 8)),])
data1 = rbind(data1 , wine[which((wine$quality == 9)),])


data = wine[which(wine$quality==5),]

for(i in 1:ncol(data[1:11]))
{  
  val = boxplot.stats(data[,i])$out 
  data = data[which(!data[,i] %in% val),] 
}

data1 = rbind(data1 , data)

data = wine[which(wine$quality==6),]

for(i in 1:ncol(data[1:11]))
{  
  val = boxplot.stats(data[,i])$out 
  data = data[which(!data[,i] %in% val),] 
}

final_data = rbind(data1 , data)
rm(data)
rm(data1)

#Creating new variable taste based on quality values
final_data$taste <- ifelse(final_data$quality < 6, 'bad', 'good')
final_data$taste[final_data$quality == 6] <- 'average'
final_data$taste <- as.factor(final_data$taste)


#Creating train and test data
set.seed(123)
samp <- sample(nrow(final_data), 0.66 * nrow(final_data))
train <- final_data[samp, ]
test <- final_data[-samp, ]

library(caret)

#Trying out naive bayes classifier
library(e1071)
model1 = naiveBayes(taste ~ . - quality, data = train)
pred <- predict(model1, newdata = test[,1:12])
table_naivebayes= table(pred, test$taste)
confusionMatrix(table_naivebayes)

#Using random forest to build a model
library(randomForest)
model2 <- randomForest((taste) ~ . - quality, data = train,ntree=1000,do.trace=100)
final_pred <- predict(model2, newdata = test[,1:13])
table_randomForest= table(final_pred, test$taste)
confusionMatrix(table_randomForest)

Accuracy = (table_randomForest[1,1]+table_randomForest[2,2]+table_randomForest[3,3])/nrow(test) *100


