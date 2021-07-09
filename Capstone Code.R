getwd()
library(plyr)
library(reshape2)

ten <- read.csv("2010.csv")
        ten$Income <- as.numeric(gsub(",",'',ten$Income))
eleven <- read.csv("2013.csv")
        eleven$Income <- as.numeric(gsub(',','',eleven$Income))
twelve <- read.csv("2012.csv")
        twelve$Income <- as.numeric(gsub(',','',twelve$Income))
thirteen <- read.csv("2013.csv")
        thirteen$Income <- as.numeric(gsub(',','',thirteen$Income))
fourteen <- read.csv("2014.csv")
        fourteen$Income <- as.numeric(gsub(',','',fourteen$Income))
fifteen <- read.csv("2015.csv")
        fifteen$Income <- as.numeric(gsub(',','',fifteen$Income))
sixteen <- read.csv("2016.csv")
        sixteen$Income <- as.numeric(gsub(',','',sixteen$Income))
seventeen <- read.csv("2017.csv")
        seventeen$Income <- as.numeric(gsub(',','',seventeen$Income))
eighteen <- read.csv("2018.csv")
        eighteen$Income <- as.numeric(gsub(',','',eighteen$Income))
nineteen <- read.csv("2019.csv")
        nineteen$Income <- as.numeric(gsub(',','',nineteen$Income))

#Cleaning up Data

ten$Region <- as.character(ten$Region)
ten$Region <- revalue(ten$Region, c("1"="Northeast", "2" = "Midwest", "3" = "Southeast",
                                          "4" = "Southwest", "5" = "Mountain", "6" = "West",
                                          "7" = "Noncontinuous"))
x10 <- aggregate(ten[,3:14], list(ten$Region), mean)

eleven$Region <- as.character(eleven$Region)
eleven$Region <- revalue(eleven$Region, c("1"="Northeast", "2" = "Midwest", "3" = "Southeast",
                                    "4" = "Southwest", "5" = "Mountain", "6" = "West",
                                    "7" = "Noncontinuous"))
x11 <- aggregate(eleven[,3:14], list(eleven$Region), mean)

twelve$Region <- as.character(twelve$Region)
twelve$Region <- revalue(twelve$Region, c("1"="Northeast", "2" = "Midwest", "3" = "Southeast",
                                          "4" = "Southwest", "5" = "Mountain", "6" = "West",
                                          "7" = "Noncontinuous"))
x12 <- aggregate(twelve[,3:14], list(twelve$Region), mean)

thirteen$Region <- as.character(thirteen$Region)
thirteen$Region <- revalue(thirteen$Region, c("1"="Northeast", "2" = "Midwest", "3" = "Southeast",
                                          "4" = "Southwest", "5" = "Mountain", "6" = "West",
                                          "7" = "Noncontinuous"))
x13 <- aggregate(thirteen[,3:14], list(thirteen$Region), mean)

fourteen$Region <- as.character(fourteen$Region)
fourteen$Region <- revalue(fourteen$Region, c("1"="Northeast", "2" = "Midwest", "3" = "Southeast",
                                          "4" = "Southwest", "5" = "Mountain", "6" = "West",
                                          "7" = "Noncontinuous"))
x14 <- aggregate(fourteen[,3:14], list(fourteen$Region), mean)

fifteen$Region <- as.character(fifteen$Region)
fifteen$Region <- revalue(fifteen$Region, c("1"="Northeast", "2" = "Midwest", "3" = "Southeast",
                                          "4" = "Southwest", "5" = "Mountain", "6" = "West",
                                          "7" = "Noncontinuous"))
x15 <- aggregate(fifteen[,3:14], list(fifteen$Region), mean)

sixteen$Region <- as.character(sixteen$Region)
sixteen$Region <- revalue(sixteen$Region, c("1"="Northeast", "2" = "Midwest", "3" = "Southeast",
                                          "4" = "Southwest", "5" = "Mountain", "6" = "West",
                                          "7" = "Noncontinuous"))
x16 <- aggregate(sixteen[,3:14], list(sixteen$Region), mean)

seventeen$Region <- as.character(seventeen$Region)
seventeen$Region <- revalue(seventeen$Region, c("1"="Northeast", "2" = "Midwest", "3" = "Southeast",
                                          "4" = "Southwest", "5" = "Mountain", "6" = "West",
                                          "7" = "Noncontinuous"))
x17 <- aggregate(seventeen[,3:14], list(seventeen$Region), mean)

eighteen$Region <- as.character(eighteen$Region)
eighteen$Region <- revalue(eighteen$Region, c("1"="Northeast", "2" = "Midwest", "3" = "Southeast",
                                          "4" = "Southwest", "5" = "Mountain", "6" = "West",
                                          "7" = "Noncontinuous"))
x18 <- aggregate(eighteen[,3:14], list(eighteen$Region), mean)

nineteen$Region <- as.character(nineteen$Region)
nineteen$Region <- revalue(nineteen$Region, c("1"="Northeast", "2" = "Midwest", "3" = "Southeast",
                                          "4" = "Southwest", "5" = "Mountain", "6" = "West",
                                          "7" = "Noncontinuous"))
x19 <- aggregate(nineteen[,3:14], list(nineteen$Region), mean)



###Creating Region Datasets###
Year <- c(2010:2019)

Midwest <- rbind(x10[1,2:13], x13[1,2:13], x12[1,2:13], x13[1,2:13], x14[1,2:13],
                 x15[1,2:13],x16[1,2:13],x17[1,2:13],x18[1,2:13],x19[1,2:13])
Midwest <- cbind(Year, Midwest)

Mountain <- rbind(x10[2,2:13], x13[2,2:13], x12[2,2:13], x13[2,2:13], x14[2,2:13],
                  x15[2,2:13],x16[2,2:13],x17[2,2:13],x18[2,2:13],x19[2,2:13])
Mountain <- cbind(Year, Mountain)

Noncontinuous <- rbind(x10[3,2:13], x13[3,2:13], x12[3,2:13], x13[3,2:13], x14[3,2:13],
                       x15[3,2:13],x16[3,2:13],x17[3,2:13],x18[3,2:13],x19[3,2:13])
Noncontinuous <- cbind(Year, Noncontinuous)

Northeast <- rbind(x10[4,2:13], x13[4,2:13], x12[4,2:13], x13[4,2:13], x14[4,2:13],
                   x15[4,2:13],x16[4,2:13],x17[4,2:13],x18[4,2:13],x19[4,2:13])
Northeast <- cbind(Year, Northeast)

Southeast <- rbind(x10[5,2:13], x13[5,2:13], x12[5,2:13], x13[5,2:13], x14[5,2:13],
                 x15[5,2:13],x16[5,2:13],x17[5,2:13],x18[5,2:13],x19[5,2:13])
Southeast <- cbind(Year, Southeast)

Southwest <- rbind(x10[6,2:13], x13[6,2:13], x12[6,2:13], x13[6,2:13], x14[6,2:13],
                   x15[6,2:13],x16[6,2:13],x17[6,2:13],x18[6,2:13],x19[6,2:13])
Southwest <- cbind(Year, Southwest)

West <- rbind(x10[7,2:13], x13[7,2:13], x12[7,2:13], x13[7,2:13], x14[7,2:13],
              x15[7,2:13],x16[7,2:13],x17[7,2:13],x18[7,2:13],x19[7,2:13])
West <- cbind(Year, West)

#Filling in Missing Values
#install.packages("smooth")
#install.packages("Mcomp")
library(smooth)
library(Mcomp)
#Binge

westmiss <- forecast(sma(West$Binge), h=1)
x19[7,3] <- westmiss$forecast
West[10,3] <- westmiss$forecast

nemiss <- forecast(sma(Northeast$Binge), h=1)
x19[4,3] <- nemiss$forecast
Northeast[10,3] <- nemiss$forecast

midmiss <- forecast(sma(Midwest$Binge), h=1)
x19[1,3] <- midmiss$forecast
Midwest[10,3] <- midmiss$forecast

semiss <- forecast(sma(Southeast$Binge), h=1)
x19[5,3] <- semiss$forecast
Southeast[10,3] <- semiss$forecast

swmiss <- forecast(sma(Southwest$Binge), h=1)
x19[6,3] <- swmiss$forecast
Southwest[10,3] <- swmiss$forecast

mnmiss <- forecast(sma(Mountain$Binge), h=1)
x19[2,3] <- mnmiss$forecast
Mountain[10,3] <- mnmiss$forecast

nonmiss <- forecast(sma(Noncontinuous$Binge), h=1)
x19[3,3] <- nonmiss$forecast
Noncontinuous[10,3] <- nonmiss$forecast


#Average Of All Years By Region
Westagg <- colMeans(West, na.rm = T)
Northeastagg <- colMeans(Northeast, na.rm = T)
Midwestagg <- colMeans(Midwest, na.rm = T)
Southeastagg <- colMeans(Southeast, na.rm = T)
Mountainagg <- colMeans(Mountain, na.rm = T)
Nonagg <- colMeans(Noncontinuous, na.rm = T)
Southwestagg <- colMeans(Southwest, na.rm = T)

###Exploratory Data Analysis###
#Binge Graph#
par(mfrow = c(1,3))
cols <- c("Black","Red","Blue","Orange","Purple","Brown","Dark Green")
regions <- c("West", "Northeast", "Midwest","Southeast",
             "Southwest","Mountain","Noncontin.")

plot(West$Year, West$Binge, type = 'l',xlim = c(2010, 2023),ylim = c(12,21),
     main = "Binge Drinking Rates (Adults) By Region", xlab = 'Year',
     ylab = 'Binge Drinking Rate')
lines(Northeast$Year, Northeast$Binge, col = 'red')
lines(Midwest$Year, Midwest$Binge, col = 'Blue')
lines(Southeast$Year, Southeast$Binge, col = 'Orange')
lines(Southwest$Year, Southwest$Binge, col = 'Purple')
lines(Mountain$Year, Mountain$Binge, col = 'Brown')
lines(Noncontinuous$Year, Noncontinuous$Binge, col = 'Dark Green')
legend("bottomright", legend = regions, col = cols, cex = 0.7, lty = 1)

#Smoking#
plot(West$Year, West$Smoking, type = 'l',xlim = c(2010, 2023),ylim = c(12,25),
     main = "Smoking Rates (Adults) By Region", xlab = 'Year',
     ylab = 'Smoking Rate')
lines(Northeast$Year, Northeast$Smoking, col = 'red')
lines(Midwest$Year, Midwest$Smoking, col = 'Blue')
lines(Southeast$Year, Southeast$Smoking, col = 'Orange')
lines(Southwest$Year, Southwest$Smoking, col = 'Purple')
lines(Mountain$Year, Mountain$Smoking, col = 'Brown')
lines(Noncontinuous$Year, Noncontinuous$Smoking, col = 'Dark Green')
legend("bottomright", legend = regions, col = cols, cex = 0.7, lty = 1)

#Income#
plot(West$Year, West$Income, type = 'l',xlim = c(2010, 2023), 
     ylim = c(40000,85000),
     main = "Average Income By Region", xlab = 'Year',
     ylab = 'Income')
lines(Northeast$Year, Northeast$Income, col = 'red')
lines(Midwest$Year, Midwest$Income, col = 'Blue')
lines(Southeast$Year, Southeast$Income, col = 'Orange')
lines(Southwest$Year, Southwest$Income, col = 'Purple')
lines(Mountain$Year, Mountain$Income, col = 'Brown')
lines(Noncontinuous$Year, Noncontinuous$Income, col = 'Dark Green')
legend("bottomright", legend = regions, col = cols, cex = 0.7, lty = 1)
par(mfrow=c(1,3))
#Air Quality#
plot(West$Year, West$Air.Quality, type = 'l',xlim = c(2010, 2023),
     ylim = c(6,12),
     main = "Air Quality By Region", xlab = 'Year',
     ylab = 'Air Quality (PM 2.5)')
lines(Northeast$Year, Northeast$Air.Quality, col = 'red')
lines(Midwest$Year, Midwest$Air.Quality, col = 'Blue')
lines(Southeast$Year, Southeast$Air.Quality, col = 'Orange')
lines(Southwest$Year, Southwest$Air.Quality, col = 'Purple')
lines(Mountain$Year, Mountain$Air.Quality, col = 'Brown')
lines(Noncontinuous$Year, Noncontinuous$Air.Quality, col = 'Dark Green')
legend("bottomright", legend = regions, col = cols, cex = 0.7, lty = 1)

#Insurance Cost#
plot(West$Year, West$Insurance.Cost, type = 'l',xlim = c(2010, 2023),
     ylim = c(6000,12500),
     main = "Insurance Costs By Region", xlab = 'Year',
     ylab = 'Average Insurance Cost')
lines(Northeast$Year, Northeast$Insurance.Cost, col = 'red')
lines(Midwest$Year, Midwest$Insurance.Cost, col = 'Blue')
lines(Southeast$Year, Southeast$Insurance.Cost, col = 'Orange')
lines(Southwest$Year, Southwest$Insurance.Cost, col = 'Purple')
lines(Mountain$Year, Mountain$Insurance.Cost, col = 'Brown')
lines(Noncontinuous$Year, Noncontinuous$Insurance.Cost, col = 'Dark Green')
legend("bottomright", legend = regions, col = cols, cex = 0.7, lty = 1)

#Uninsured#
plot(West$Year, West$Uninsured, type = 'l',xlim = c(2010, 2023),
     ylim = c(5,22),
     main = "Percent Uninsured By Region", xlab = 'Year',
     ylab = 'Percent Uninsured')
lines(Northeast$Year, Northeast$Uninsured, col = 'red')
lines(Midwest$Year, Midwest$Uninsured, col = 'Blue')
lines(Southeast$Year, Southeast$Uninsured, col = 'Orange')
lines(Southwest$Year, Southwest$Uninsured, col = 'Purple')
lines(Mountain$Year, Mountain$Uninsured, col = 'Brown')
lines(Noncontinuous$Year, Noncontinuous$Uninsured, col = 'Dark Green')
legend("bottomright", legend = regions, col = cols, cex = 0.7, lty = 1)
par(mfrow=c(1,3))
#Obesity#
plot(West$Year, West$Obesity, type = 'l',xlim = c(2010, 2023),
    ylim = c(24,35),
     main = "Obesity Rate By Region", xlab = 'Year',
     ylab = 'Percent Obese')
lines(Northeast$Year, Northeast$Obesity, col = 'red')
lines(Midwest$Year, Midwest$Obesity, col = 'Blue')
lines(Southeast$Year, Southeast$Obesity, col = 'Orange')
lines(Southwest$Year, Southwest$Obesity, col = 'Purple')
lines(Mountain$Year, Mountain$Obesity, col = 'Brown')
lines(Noncontinuous$Year, Noncontinuous$Obesity, col = 'Dark Green')
legend("bottomright", legend = regions, col = cols, cex = 0.7, lty = 1)

#Diabetes#
plot(West$Year, West$Diabetes, type = 'l',xlim = c(2010, 2023),
     ylim = c(6,14),
     main = "Diabetes Rate By Region", xlab = 'Year',
     ylab = 'Diabetes Rate')
lines(Northeast$Year, Northeast$Diabetes, col = 'red')
lines(Midwest$Year, Midwest$Diabetes, col = 'Blue')
lines(Southeast$Year, Southeast$Diabetes, col = 'Orange')
lines(Southwest$Year, Southwest$Diabetes, col = 'Purple')
lines(Mountain$Year, Mountain$Diabetes, col = 'Brown')
lines(Noncontinuous$Year, Noncontinuous$Diabetes, col = 'Dark Green')
legend("bottomright", legend = regions, col = cols, cex = 0.7, lty = 1)

#HBP#
plot(West$Year, West$High.Blood.Pressure, type = 'l',xlim = c(2010, 2023),
     ylim = c(25,38),
     main = "High Blood Pressure Rate By Region", xlab = 'Year',
     ylab = 'High Blood Pressure Rate')
lines(Northeast$Year, Northeast$High.Blood.Pressure, col = 'red')
lines(Midwest$Year, Midwest$High.Blood.Pressure, col = 'Blue')
lines(Southeast$Year, Southeast$High.Blood.Pressure, col = 'Orange')
lines(Southwest$Year, Southwest$High.Blood.Pressure, col = 'Purple')
lines(Mountain$Year, Mountain$High.Blood.Pressure, col = 'Brown')
lines(Noncontinuous$Year, Noncontinuous$High.Blood.Pressure, col = 'Dark Green')
legend("bottomright", legend = regions, col = cols, cex = 0.7, lty = 1)

par(mfrow=c(1,1))
#Regression
library(MASS)
library(glmnet)
library(plotmo)

#West Regression#
westreg <- West[,-c(1,5,9,11)]
cv.lasso1 <- cv.glmnet(as.matrix(westreg[-4]), as.matrix(westreg[4]), 
                     alpha = 1, nfolds = 3, type.measure = 'mse')
cv.5 <- cv.glmnet(as.matrix(westreg[-4]), as.matrix(westreg[4]), 
                  alpha = 0.5, nfolds = 3, type.measure = 'mse')
cv0 <- cv.glmnet(as.matrix(westreg[-4]), as.matrix(westreg[4]), 
                 alpha = 0, nfolds = 3, type.measure = 'mse')

plot(log(cv.lasso1$lambda), cv.lasso1$cvm , pch = 19, col = "red",
     xlab = "log(Lambda)", ylab = cv.lasso1$name,
     main = 'Lasso vs Ridge vs Elastic Regression')
points(log(cv.5$lambda), cv.5$cvm, pch = 19, col = "grey")
points(log(cv0$lambda) , cv0$cvm , pch = 19, col = "blue")
legend("topleft", legend = c("alpha= 1", "alpha= .5", "alpha 0"),
       pch = 19, col = c("red","grey","blue"))
###We can see from this that lasso regression is the best method#
cv.lasso1
plot(cv.lasso1)


model2 <- glmnet(as.matrix(westreg[,-4]), as.matrix(westreg[,4]), 
                 family = 'gaussian', alpha = 1)
summary(model2)
coef(model2)
par(mfrow=c(3,3))
plot_glmnet(model2, label = 5, main = 'Leading Factors in the West Region')
    abline(v = log(cv.lasso1$lambda.min), lty = 'dashed')


#Northeast Regression#
northreg <- Northeast[,-c(1,5,9,11)]
cv.lasso2 <- cv.glmnet(as.matrix(northreg[-4]), as.matrix(northreg[4]), 
                   alpha = 1, nfolds = 3, type.measure = 'mse')
model3 <- glmnet(as.matrix(northreg[,-4]), as.matrix(northreg[,4]), 
                 family = 'gaussian', alpha = 1)
summary(model3)
coef(model3)
plot_glmnet(model3, label =4, main = 'Leading Factors in the Northeast Region')
    abline(v = log(cv.lasso2$lambda.min), lty = 'dashed')

#Midwest Regression#
midwestreg <- Midwest[,-c(1,5,9,11)]
cv.lasso3 <- cv.glmnet(as.matrix(midwestreg[-4]), as.matrix(midwestreg[4]), 
                       alpha = 1, nfolds = 3, type.measure = 'mse')
model4 <- glmnet(as.matrix(midwestreg[,-4]), as.matrix(midwestreg[,4]), 
                 family = 'gaussian', alpha = 1)
summary(model4)
coef(model4)
plot_glmnet(model4, main = 'Leading Factors in the Midwest Region', label = 3)
    abline(v = log(cv.lasso3$lambda.min), lty = 'dashed')

#Mountain Regression#
mountreg <- Mountain[,-c(1,5,9,11)]
cv.lasso4 <- cv.glmnet(as.matrix(mountreg[-4]), as.matrix(mountreg[4]), 
                       alpha = 1, nfolds = 3, type.measure = 'mse')
model5 <- glmnet(as.matrix(mountreg[,-4]), as.matrix(mountreg[,4]), 
                 family = 'gaussian', alpha = 1)
summary(model5)
coef(model4)
plot_glmnet(model5, main = 'Leading Factors in the Mountain Region', label = 4)
    abline(v = log(cv.lasso4$lambda.min), lty = 'dashed')

#Noncontinuous Regression#
nonreg <- Noncontinuous[,-c(1,5,9,11)]
cv.lasso5 <- cv.glmnet(as.matrix(nonreg[-4]), as.matrix(nonreg[4]), 
                       alpha = 1, nfolds = 3, type.measure = 'mse')
model6 <- glmnet(as.matrix(nonreg[,-4]), as.matrix(nonreg[,4]), 
                 family = 'gaussian', alpha = 1)
summary(model6)
coef(model6)
plot_glmnet(model6, main = 'Leading Factors in the Noncontinuous Region', label = 7)
    abline(v = log(cv.lasso5$lambda.min), lty = 'dashed')

#Southeast Regression#
southeastreg <- Southeast[,-c(1,5,9,11)]
cv.lasso6 <- cv.glmnet(as.matrix(southeastreg[-4]), as.matrix(southeastreg[4]), 
                       alpha = 1, nfolds = 3, type.measure = 'mse')
model7 <- glmnet(as.matrix(southeastreg[,-4]), as.matrix(southeastreg[,4]), 
                 family = 'gaussian', alpha = 1)
summary(model7)
coef(model7)
plot_glmnet(model7, main = 'Leading Factors in the Southeast', label = 4)
    abline(v = log(cv.lasso6$lambda.min), lty = 'dashed')

#Southwest Regression#
southwestreg <- Southwest[,-c(1,5,9,11)]
cv.lasso7 <- cv.glmnet(as.matrix(southwestreg[-4]), as.matrix(southwestreg[4]), 
                       alpha = 1, nfolds = 3, type.measure = 'mse')
model8 <- glmnet(as.matrix(southwestreg[,-4]), as.matrix(southwestreg[,4]), 
                 family = 'gaussian', alpha = 1)
summary(model8)
coef(model8)
plot_glmnet(model8, main = 'Leading Factors in the Southwest', label = 6)
    abline(v = log(cv.lasso7$lambda.min), lty = 'dashed')
par(mfrow=c(1,1))

#Machine Learning#
x10
x11
x12
combined <- rbind(x10,x11,x12,x13,x14,x15,x16,x17,x18,x19)
combined <- combined[order(combined$Group.1),]
combined <- cbind(combined, Year = rep(2010:2019, 7))
combined <- combined[c(1,14,2,3,4,6,7,8,9,10,11,12,13)]
colnames(combined)[1] <- 'Region'
combined3 <- combined
combined <- combined[,-c(2,9,11)]

n <- nrow(combined)
p <- ncol(combined)-1
pos <- 1
x <- combined[,-1]
y <- combined[,1]


library(caret)
library(psych)
library(class)
# indxtrain <- createDataPartition(y = combined$Region,
#                                  p=0.9, list = F)
# training <- combined[indxtrain,]
# testing <- combined[-indxtrain,]
# prop.table(table(training$Region))*100
# 
# set.seed(400)
# trcontrol <- trainControl(method = "cv",
#                           number = 10)
# grid1 <- expand.grid(.k=seq(1,20, by=1))
# 
# fit <- train(Region~., data = training,
#              method = "knn",
#              tuneGrid = grid1,
#              trControl = trcontrol)
# plot(fit)
# fit
# bestk <-  5 #4 is the optimal number of nearest neighbors#
# 
# combinedmodel <- knn(x[indxtrain,], x[-indxtrain,], y[indxtrain], k=bestk)
# conmatrix <- table(y[-indxtrain], combinedmodel)
# conmatrix
# 
# #Accuracy Rate#
# sum(diag(conmatrix) / sum(conmatrix))*100
# 
# #Error Rate#
# (1-sum(diag(conmatrix) / sum(conmatrix)))*100

#####################
library(corrplot)
library(cluster)
library(factoextra)

corrplot(cor(x))

combined5 <- combined[,-1]
combined5 <- scale(combined5)
distance <- get_dist(combined5)
fviz_dist(distance, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#2010 Cluster#
par(mfrow = c(1,3))
rownames(x10) <- x10$Group.1
x10.1 <- scale(x10[,-c(1,5)])
dist1 <- get_dist(x10.1, method = 'euclidian')
#fviz_dist(dist1, 
         #gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
hclust.xx <- hclust(dist1, method = 'ward.D')
cut.k <- cutree(hclust.xx, k=7) #nb.c is the number of desirable clusters
label <- rownames(as.data.frame(cut.k))
plot(hclust.xx, labels = label, ylab = 'Distance',
    main = '2010 Cluster Analysis Dendogram', xlab = '2010 Clusters')
rect.hclust(hclust.xx, k=3, border = 'red')#REGIONS DID CLUSTER TOGETHER

#2015 Cluster#
rownames(x15) <- x15$Group.1
x15.1 <- scale(x15[,-c(1,5)])
dist3 <- get_dist(x15.1, method = 'euclidian')
#fviz_dist(dist3, 
         # gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
hclust.xx3 <- hclust(dist3, method = 'ward.D')
cut.k3 <- cutree(hclust.xx3, k=7) #nb.c is the number of desirable clusters
label3 <- rownames(as.data.frame(cut.k3))
plot(hclust.xx, labels = label3, ylab = 'Distance',
    main = '2015 Cluster Analysis Dendogram', xlab = '2015 Clusters')
rect.hclust(hclust.xx, k=3, border = 'red')

#2019 Cluster#
rownames(x19) <- x19$Group.1
x19.1 <- scale(x19[,-c(1,5)])
dist2 <- get_dist(x19.1, method = 'euclidian')
#fviz_dist(dist2, 
          #gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
hclust.xx2 <- hclust(dist2, method = 'ward.D')
cut.k2 <- cutree(hclust.xx2, k=7) #nb.c is the number of desirable clusters
label2 <- rownames(as.data.frame(cut.k2))
plot(hclust.xx2, labels = label2, ylab = 'Distance', 
     main = '2019 Cluster Analysis Dendogram', xlab = '2019 Clusters')
rect.hclust(hclust.xx2, k=3, border = 'red')
par(mfrow = c(1,1))


###Cross Correlation coefficient###
combined4 <- as.matrix(combined[,-1])
class(combined4)

combins <- combn(colnames(combined4), 2)

a1 <- apply(combins, 2,
            FUN = function(x){ccf(combined[, x[1]], combined[, x[2]])})

abs_max_ccf <- unlist(lapply(a1, function(x) abs(max(x$acf))))

names(abs_max_ccf) <- apply(combins, 2, function(x) paste0(x[1], 
                                x[2], collapse = ''))

ccfs <- as.matrix(abs_max_ccf)
ccfs <- ccfs[order(ccfs[,1], decreasing = T),]
correlation <- as.data.frame(head(round(ccfs, 2), 10))
colnames(correlation) <- 'Cross Correlation Coefficients'
correlation

