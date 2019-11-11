# Name: Beatriz Guerra
# Capstone Project: Analysis of the sales in the LiveFresh Ltd Company.

# Libraries and packages
library(xlsx)
library(FSelector)
library(corrplot)
library(gtools)
#Install Caret package
#Install mlbench package
library(mlbench)
library(caret)

#************* SECTION 1: Data preparation ************************

url1 = "/home/beatriz/Desktop/Capstone_Project_136/Refreshment/refreshment-sales/OK/merged_data.xlsx"
data = read.xlsx(url1, sheetIndex = 1, header = TRUE)
data1 = data[,0:13]
head(data1)
str(data1)

# Converting the variable "sales" from num to categorical and numeric levels
var_sales = data1[,"sales"]
sales_categ = quantcut(var_sales, q=3, labels= c("Low", "Normal", "High"))
sales_num = quantcut(var_sales, q=3, labels= c(1,2,3))
table(sales_categ)

# Data set with categorical dependent variable
data3_categ = data1[,c(-1,-8,-9)] # Eliminate the num variable "sales" because I have the new categorical variable that desribe sales
data3_categ["Sales_Category"] = sales_categ

# Data set with num dependent variable
data3_num = data1[,c(-1,-8,-9)]
data3_num["Sales_Num_Category"] = as.numeric(sales_num)


#Dummy variables (Converting all categorical variables to numeric)
# Season
levels(data3_num$Season)<-list('winter'=1, 'spring'=2, 'summer'=3, 'fall'=4)
data3_num$Season<-as.numeric(data3_num$Season)
# Shops
levels(data3_num$shop)<-list('shop_1'=1, 'shop_2'=2, 'shop_3'=3, 'shop_4'=4, 'shop_5'=5, 'shop_6'=6)
data3_num$shop<-as.numeric(data3_num$shop)
# Brand
levels(data3_num$brand)<-list('kinder-cola'=1, 'adult-cola'=2, 'orange-power'=3, 'lemon-boost'=4, 'gazoza'=5)
data3_num$brand<-as.numeric(data3_num$brand)
# Container
levels(data3_num$container)<-list('can'=1, 'plastic'=2, 'glass'=3)
data3_num$container<-as.numeric(data3_num$container)
# Capacity
levels(data3_num$capacity..ml.)<-list('330ml'=1, '500ml'=2, '1500ml'=3)
data3_num$capacity..ml.<-as.numeric(data3_num$capacity..ml.)
# City
levels(data3_num$city)<-list('Athens'=1, 'Heraklion'=2, 'Thessaloniki'=3, 'Patras'=4, 'Larissa'=5)
data3_num$city<-as.numeric(data3_num$city)

# NOTE: data3_num is a data set with all numeric variables



# Normalize my data

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data3_num$quantity = normalize(data3_num$quantity)
data3_num$longitude  = normalize(data3_num$longitude)
data3_num$latitud = normalize(data3_num$latitud)
data3_num$Population = normalize(data3_num$Population)
str(data3_num)

# Converting dependent variable as categorical

data4_depV_categ = data3_num [, -11]
data4_depV_categ["Sales_Category"] = sales_categ



#*************SECTION 2: Feature Selection************************

### Filter Methods ######

#Using information gain
information.gain(sales~., data1) # Without transformation
information.gain(Sales_Num_Category~., data3_num) #as numeric

information.gain(Sales_Category~., data4_depV_categ) # numeric attributes and categ dependent variable 

#Using correlation matrix (ONLY FOR NUMERIC VARIABLES)

corr = cor(data4_depV_categ[,-11])
corrplot(corr, method = "number")


#### Wrapper Methods ######

#Select features using Caret R Package

set.seed(10)
# Define the control using a random forest
control_rf <- rfeControl(functions=rfFuncs, method="cv", number=10) #This function generates a control object that can 
# be used to specify the details of the feature selection algorithms used in this package.

# Recursive feature elimination (RFE) algorithm (Backwards feature selection)(NUM)
results_rfe <- rfe(data4_depV_categ[,1:10], data4_depV_categ[,11], sizes=c(1:10), rfeControl=control_rf)
print(results_rfe)

# list the chosen features
predictors(results_rfe)
plot(results_rfe, type=c("g", "o"))



#### Calculate variable importance using linear regression

# regression model
r_model = lm(Sales_Category~.,data4_depV_categ, type = 'numeric' )

summary(r_model)

#using Carpet package

importance = varImp(r_model)
importance

#### Calculate variable importance using random forest library  to compare

library(randomForest)
rf_model = randomForest(Sales_Category~., data=data4_depV_categ)
# Importance
importance(rf_model)
attr_importance = varImpPlot(rf_model, scale=F)
print(attr_importance)
attr_importance

plot(attr_importance, top = 20, main='Variable Importance')

# write.table(data3_num, "/home/beatriz/Desktop/Capstone_Project_136/Refreshment/refreshment-sales/OK/final_dataset.csv", row.names = FALSE)
# write.table(data4_depV_categ, "/home/beatriz/Desktop/Capstone_Project_136/Refreshment/refreshment-sales/OK/final_dataset_dVcateg.csv", row.names = FALSE)


#************* SECTION 3: Classification techniques ************************

# Preparing the data to implement a Random forest classifier

final_data = data4_depV_categ[, c(-2,-8,-9)]
str(final_data)

# Training and Test sets (70:30)

set.seed(100)
train = sample(nrow(final_data), 0.7*nrow(final_data), replace = FALSE)
str(train)
train_set = final_data[train,]
test_set = final_data[-train,]
str(train_set)
str(test_set)

# Random Forest model

model_rf_train = randomForest(Sales_Category~ ., data = train_set, importance = TRUE)
summary(model_rf) 


#Predicting and evaluation of the model

pred = predict(model_rf_train, train_set, type = "class")

table(pred, train_set$Sales_Category)  
confusionMatrix(pred, train_set$Sales_Category)
