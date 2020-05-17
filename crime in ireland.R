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

#this is the graph of changes in crime over the years(divided in quarters) based on counties.
crime %in%
  select(Region,No.of.offences) %in%
  filter(crime$County) %in%
  group_by(Regions) %in%
  summarize(TotalIncidents = sum(No.of.offences, na.rm=TRUE)) %in% arrange(desc(TotalIncidents))

write.csv("Crime - 2007-2014.csv", file = "CrimeData.csv")

# plot of correlations
install.packages("corrplot")
library(corrplot)
# remove dimensions, categorical and unwanted variables
crimeCorr = crime[,c(-1,-2,-3,-4,-21)]
viewcorr = crimeCorr
M<-cor(viewcorr)
summary(viewcorr)
str(viewcorr)
corrplot(M, method="circle")
corrplot.mixed(M)
#The structured is changed 
str(crime)


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
 
# MAPE
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals)) / actuals_preds$actuals)
mape



# Principal Component Analysis (PCA) wrks best with numerical data
# so I'm checking that all data is now numeric first

data_numeric <- sapply(crime, is.numeric)
data_numeric 
data_file_adjusted <- crime[, data_numeric]
pca <- prcomp(data_file_adjusted, center = TRUE, scale. = TRUE)
summary(pca)

str(pca)
library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values


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
# Contribution to PC1 - PC5
fviz_contrib(pca, choice = "var", axes = 1:5, top = 20)
biplot <- fviz_pca_ind(pca, geom = "point", col.ind = crime$Region)
ggpubr::ggpar(biplot,
              title = "Principal Component Analysis",
              subtitle = "Crime In Ireland  dataset",
              caption = "Source: BBC",
              xlab = "PC 1", ylab = "PC 2",
              legend.title = "Vote", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco")
# Lets see how PC 3 and PC 4 represent voters data.
biplot <- fviz_pca_ind(pca, 
                       axes = c(3, 4),
                       geom = "point", 
                       col.ind = crime$Region)
ggpubr::ggpar(biplot,
              title = "Principal Component Analysis",
              subtitle = "Crime In Ireland  dataset",
              caption = "Source: BBC",
              xlab = "PC 3", ylab = "PC 4",
              legend.title = "Vote", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco")
#Hypothesis Testing
#Hypothesis Testing
test <- cor.test(data_file_adjusted$No_of_Offences...,data_file_adjusted$No_of_Offences,
                 method = 'spearman', exact = FALSE) 

test

#hypothesis testing

results21 <- vector(mode = "numeric")
for(i in 1:10000) {
  tempSample <- sample(c("L", "R"), size = 27, prob = c(0.25, 0.75), replace = TRUE)
  results21[i] <- sum(tempSample == "L")
}
results21fac <- factor(results21, levels = 0:21)
nullTable <- table(results21fac, dnn = "data_file_adjusted")/length(results21fac)
data.frame(nullTable)
hist(results21, right = FALSE, freq = FALSE, xlab = "Number of Offences")
barplot(height = nullTable, space = 0, las = 1, cex.names = 0.8, col = "blue",
        xlab = "Number of Offences", ylab = "Relative frequency")
frac6orLess <- sum(results21 <= 6)/length(results21)
frac6orLess
2 * frac6orLess


