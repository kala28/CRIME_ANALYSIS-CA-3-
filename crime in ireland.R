#crime dataset is downloaded in .csv format and the project is created and pushed into git link.
#the following steps have been done while analysisng the data.

#get the working directory to make sure verything is in the same folder.
getwd()
rm(list=ls())
#read the dataset.
crime=read.csv('Crime - 2007-2014.csv',header=TRUE)
#structure of dataset.
str(crime)
#summary of dataset.
summary(crime)

#To check whether there are any  missing values, if found to delete if it’s unnecessary.
# this gives a list of all features and their percentage of missing values.
missingvalues<-function(crime)
  {sum(is.na(crime))/length(crime)*100}
apply(crime,2,missingvalues)


is.na(crime $Num_Practising_Religion)
is.na(crime $Pupils_Finish_Leaving_Cert)

#Now by the output, we came to know that,Pupils_Finish_Leaving_Cert and Num_Practising_Religion has large number of missing values.

#drop the 2 variables with high proportions of missing values
crime = crime[,c(-19,-20)]
str(crime)

#As a part of data analysis, answering one of research question is , finding the status of crimes over the years which is divided in quarters grouped by counties.
library("lattice")
histogram(~No_of_Offences| Region, data = crime)
#this is the graph of changes in crime over the years(divided in quarters) based on counties.
crime %in%
  select(Region,No.of.offences) %in%
  filter(crime$County) %in%
  group_by(Regions) %in%
  summarize(TotalIncidents = sum(No.of.offences, na.rm=TRUE)) %in% arrange(desc(TotalIncidents))

write.csv("Crime - 2007-2014.csv", file = "CrimeData.csv")


#The definition of the most possibly dependent variables in order to explain the connections between the variables. A histogram of the 10 main variables is generated which would be used to test different models.
# historgrams to check distribution & log transformation
hist(crime$No_of_Offences, prob=T,xlab="No of Offences", 
     ylab="Frequency",main='Histogram of No of Offences')
crime$No_of_Offences = log10(crime$No_of_Offences +1)

hist(crime$Population_Num, prob=T,xlab="Population", 
     ylab="Frequency",main='Histogram of Population Number')
crime$Population_Num = log10(crime$Population_Num)

hist(crime$Live_Register_Num, prob=T,xlab="Live Register Num", 
     ylab="Frequency",main='Histogram of Live Register Num')
crime$Live_Register_Num = log10(crime$Live_Register_Num)

hist(crime$Mean_Disposable_Income, prob=T,xlab="Mean Disp Income", 
     ylab="Frequency",main='Histogram of Mean Disposable Income')
crime$Mean_Disposable_Income = log10(crime$Mean_Disposable_Income)

hist(crime$Rent_Rates, prob=T,xlab="Rent Rates", 
     ylab="Frequency",main='Histogram of Rent Rates')
crime$Rent_Rates = log10(crime$Rent_Rates)

hist(crime$CPI_All_Items, prob=T,xlab="CPI", 
     ylab="Frequency",main='Histogram of CPI All Items')

hist(crime$GBP_per_Euro, prob=T,xlab="GPB per Eur", 
     ylab="Frequency",main='Histogram of GPB per Eur')
crime$GBP_per_Euro = log10(crime$GBP_per_Euro +1)

hist(crime$Consistent_Poverty_Rate, prob=T,xlab="Consistent Poverty Rate", 
     ylab="Frequency",main='Histogram of Consistent Poverty Rate')
crime$Consistent_Poverty_Rate = log10(crime$Consistent_Poverty_Rate)

hist(crime$Daily_Internet_Users, prob=T,xlab="Internet Users", 
     ylab="Frequency",main='Histogram of % Daily Internet Users')
crime$Daily_Internet_Users = log10(crime$Daily_Internet_Users)

hist(crime$Pop_In_Good_Health, prob=T,xlab="Pop in Good Health", 
     ylab="Frequency",main='Histogram of Pop in Good Health')
crime$Pop_In_Good_Health = log10(crime$Pop_In_Good_Health +1)
 
 


#-----------------------------------------------------------PCA-----------------------------------------------------------#
# Principal Component Analysis (PCA) wrks best with numerical data
#checking that all data is numeric.
data_numeric <- sapply(crime, is.numeric)
data_numeric 
data_file_adjusted <- crime[, data_numeric]
pca <- prcomp(data_file_adjusted, center = TRUE, scale. = TRUE)
summary(pca)

str(pca)
install.packages("factoextra")
library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values

install.packages("FactoMineR")
library("FactoMineR")
pca2 <- PCA(data_file_adjusted, graph = FALSE)
print(pca2)
pca2_eig_values <- get_eigenvalue(pca2)
pca2_eig_values
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))
pca_for_variables <- get_pca_var(pca)
pca_for_variables
library("corrplot")
corrplot(pca_for_variables$cos2, is.corr = FALSE)
fviz_pca_var(pca, col.var = "black")
# Cos2 - quality of representation
# -----------------------------------------------------------------------
# The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates). 
# We can access to the cos2 as follows:
head(pca_for_variables$cos2, 10)
# We can show a bar plot of variables cos2 using the function fviz_cos2()
# from the in factoextra library.

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(pca, choice = "var", axes = 1:2)
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("red", "Blue", "Green"), 
             repel = TRUE # Avoid text overlapping
)  
# Contribution of variables to each PC
# The larger the value of the contribution, the more the variable contributes to the component. 
head(pca_for_variables$contrib, 20)

# The most important (or, contributing) variables can be highlighted on the correlation plot as follows
fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("red", "Blue", "Green"),
)
install.packages("factoextra")
library(factoextra)
?fviz_contrib
fviz_contrib(pca, choice = "var", axes = 1, top = 20)

# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 20)
# Contribution to PC1 - PC2
fviz_contrib(pca, choice = "var", axes = 1:5, top = 20)
biplot <- fviz_pca_ind(pca, geom = "point", col.ind = crime$Region)
ggpubr::ggpar(biplot,
              title = "Principal Component Analysis",
              subtitle = "Crime In Ireland  dataset",
              xlab = "PC 1", ylab = "PC 2",
              legend.title = "Vote", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco")
# Lets see how PC 3 and PC 4 represent crime data.
biplot <- fviz_pca_ind(pca, 
                       axes = c(3, 4),
                       geom = "point", 
                       col.ind = crime$Region)
ggpubr::ggpar(biplot,
              title = "Principal Component Analysis",
              subtitle = "Crime In Ireland  dataset",
              xlab = "PC 3", ylab = "PC 4",
              legend.title = "Vote", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco")
# Lets see how PC 4 and PC 5 represent voters data.
biplot <- fviz_pca_ind(pca, 
                       axes = c(4, 5),
                       geom = "point", 
                       col.ind = crime$Region)
ggpubr::ggpar(biplot,
              title = "Principal Component Analysis",
              subtitle = "Crime In Ireland  dataset",
              xlab = "PC4", ylab = "PC 5",
              legend.title = "Vote", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco")

#Hypothesis Testing

#Hypothesis Testing
test <- cor.test(data_file_adjusted$No_of_Offences...,data_file_adjusted$No_of_Offences,
                 method = 'spearman', exact = FALSE) 

test
#__________________________________________HYPOTHESIS TESTING___________________________________________________________________#
#hypothesis testing.
results21 <- vector(mode = "numeric")
for(i in 1:10000) {
  tempSample <- sample(c("L", "R"), size = 27, prob = c(0.25, 0.75), replace = TRUE)
  results21[i] <- sum(tempSample == "L")
}
results21fac <- factor(results21, levels = 0:21)
#tabulate the relative frequency distribution
nullTable <- table(results21fac, dnn = "data_file_adjusted")/length(results21fac)
data.frame(nullTable)
hist(results21, right = FALSE, freq = FALSE, xlab = "Number of Offences")

barplot(height = nullTable, space = 0, las = 1, cex.names = 0.8, col = "blue",
        xlab = "Number of Offences", ylab = "Relative frequency")
#calcu
frac6orLess <- sum(results21 <= 6)/length(results21)
frac6orLess
#________________________________________PREDICTING_MODELS_______________________________________________________
# k-fold cross validation
install.packages('DAAG')
library(DAAG)
cvResults <- suppressWarnings(CVlm(model3, form.lm=No_of_Offences~. -1, m=10, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')
# editing tools:
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
par
#________________________________LOGISTIC REGRESSION________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
head(crime)
#the proportion of events and non-events in the Y variable should approximately be the same. 
#So, lets first check the proportion of classes in the dependent variable No_of_offences.
table(crime $ No_of_Offences)
#Create Training and Test Samples
# Create Training Data
input_ones <- crime[which(crime $ No_of_Offences == 1), ]  # all 1's
input_zeros <- crime[which(crime $ No_of_Offences == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_zeros))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros) 
# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 
#Build Logit Models and Predict
logitMod <- glm(No_of_Offences ~  Live_Register_Num + Rent_Rates + Daily_Internet_Users + Pop_In_Good_Health, data=trainingData, family=binomial(link="logit"))
predicted <- plogis(predict(logitMod, testData))
#tuning the model# predicted scores
install.packages("InformationValue") 
library(InformationValue)
optCutOff <- optimalCutoff(crime $ No_of_Offences, predicted)[1] 
optCutOff
#Model Diagnostics
summary(logitMod)
#VIF
#Like in case of linear regression, 
#we should check for multicollinearity in the model.
#install car package inorder ot use vif function.
install.packages("car")
library(car)
vif(logitMod)
#Misclassification Error
#Misclassification error is the percentage mismatch of predcited vs actuals, 
#irrespective of 1’s or 0’s. The lower the misclassification error, the better is your model.
misClassError(crime $ No_of_Offences, predictedScores, threshold = optCutOff)
#ROC
#Receiver Operating Characteristics
#install ggplot 2 inroder to use the plotroc function
library(ggplot2)
install.packages("plotROC")
library(plotROC)
plotROC(No_of_Offences, predicted)
plotROC
#
install.packages("concordance")
library(concordance)
Concordance(crime $ No_of_Offences, predicted)

Concordance(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)
sensitivity(testData$No_of_Offences, predicted, threshold = optCutOff)

specificity(testData$No_of_Offences, predicted, threshold = optCutOff)
confusionMatrix(testData$No_of_Offences, predicted, threshold = optCutOff)
calculateConfusionMatrix(pred)
#____________________________MLR____________________________________________________________________


# try some different models
model1 = crime[,c(-1,-2,-3,-4,-11,-13,-14,-15,-16,-19,-20,-21,-22,-23)] # remove columns
model2 = model1[,c(-4,-6,-7,-8)]
model3 = model2[,c(-2)]


# outlier detection
source("http://goo.gl/UUyEzD")
outlier(model1, Pop_In_Good_Health)


# try Stepwise Regression
library(MASS)
fit <- lm(No_of_Offences~.,data=model1)
step <- stepAIC(fit, direction="both")
step$anova # display results


#MLR modelling
m1=lm(No_of_Offences~. -1,data=model1)
plot(model1$No_of_Offences,predict(m1,data=model1),xlab="Real Crime", ylab="Predicted Crime")
abline(0,1)
summary(m1)
plot(m1)
rms=sqrt((sum((crime$No_of_Offences-predict(m1,data=crime))^2))/length(crime$No_of_Offences))

m2=lm(No_of_Offences~. -1,data=model2)
plot(model2$No_of_Offences,predict(m2,data=model2),xlab="Real Crime", ylab="Predicted Crime")
abline(0,1)
summary(m2)
plot(m2)
rms=sqrt((sum((crime$No_of_Offences-predict(m2,data=crime))^2))/length(crime$No_of_Offences))

m3=lm(No_of_Offences~. -1,data=model3)
plot(model3$No_of_Offences,predict(m3,data=model3),xlab="Real Crime", ylab="Predicted Crime")
abline(0,1)

summary(m3)
plot(m3)
rms=sqrt((sum((crime$No_of_Offences-predict(m3,data=crime))^2))/length(crime$No_of_Offences))

# setting seed to reproduce results of random sampling
set.seed(100)
# row indices for training data
trainingRowIndex <- sample(1:nrow(model3), 0.8*nrow(model3))
trainingData <- model3[trainingRowIndex, ]  # model training data
testData  <- model3[-trainingRowIndex, ]   # test data

lmModel3 <- lm(No_of_Offences~. -1, data=trainingData)  # build the model
offencesPred <- predict(lmModel3, testData)  # predict No_of_Offences

summary(lmModel3)
AIC(lmModel3)

# check actuals vs predicted values
actuals_preds <- data.frame(cbind(actual=testData$No_of_Offences, predicted=offencesPred))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)


correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

plot(crime[5:24])

