# 
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
fc_data = read.xlsx(url1, sheetIndex = 1, header = TRUE)
fc_data_1 = fc_data[,0:13]
head(fc_data_1)
str(fc_data_1)

###### Analising distribution

fc_data_TEST = fc_data_1


# Anderson-Darling test (works with bigger samples):
library(nortest)
ad.test(fc_data_TEST$sales)
hist(fc_data_TEST$sales)

# p value is low (2.2e-16), so we conclude that the data do not follow the normal distribution

##### Normalizing the variable sales ,then get the type of distribution

#fc_data_TEST$sales = (fc_data_TEST$sales + abs(min(fc_data_TEST$sales)))/max(fc_data_TEST$sales)
fc_data_TEST$sales = (fc_data_TEST$sales + abs(min(fc_data_TEST$sales))+0.001)/(max(fc_data_TEST$sales)+ abs(min(fc_data_TEST$sales))+0.002)

###### Fit a distribution

library(fitdistrplus)
fit_w  <- fitdist(fc_data_TEST$sales, "weibull")
fit_g  <- fitdist(fc_data_TEST$sales, "gamma")
fit_ln <- fitdist(fc_data_TEST$sales, "lnorm")
summary(fit_ln)

# fit_p  <- fitdist(fc_data_TEST$sales, "pois", method = "mge")
# fit_nb <- fitdist(fc_data_TEST$sales, "nbinom")

####  plot dist

par(mfrow=c(2,2))
plot.legend <- c("Weibull","gamma", "lognormal")
denscomp(list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
cdfcomp (list(fit_w,fit_g, fit_ln), legendtext = plot.legend)
qqcomp  (list(fit_w,fit_g, fit_ln), legendtext = plot.legend)
ppcomp  (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)


#***** My data has a lognorm distribution******* (use this: https://cran.r-project.org/web/packages/fitdistrplus/vignettes/paper2JSS.pdf)


fc_data_2 = fc_data_TEST[,c(-1,-8)] # removing some Date and Capacity2 variables
str(fc_data_2)
mixed_df = fc_data_TEST[,c(-1,-8)]
qcut = quantcut(mixed_df$sales, q=5, labels= c("level_1", "level_2", "level_3", "level_4", "level_5"))
mixed_df["sales_category"] = qcut

# Dataset ready to use for feature selection
mixed_df = mixed_df[,-7]


#*************SECTION 2: Feature Selection************************

### Filter Methods ######

#Using information gain
info_g = information.gain(sales_categories~., fc_data_2_salesCateg) # Without transformation
plot(info_g)
info_g
#Using correlation matrix (ONLY FOR NUMERIC VARIABLES)

corr = cor(mixed_df[,c(5,8,9,10)], method = "pearson")
corrplot(corr, method = "number")
ggcorr(mixed_df[,c(5,8,9,10)])

# Using numeric variables - Dummy Dataset
corr <- cor(fc_data_2)
library("Hmisc")
corr2 <- rcorr(as.matrix(fc_data_2))
corrplot(corr, method= "number")
library(ggcorrplot)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)


#### Wrapper Methods ######

#Select features using Caret R Package
fc_data_2_salesCateg = fc_data_2_salesCateg[, -7]
set.seed(10)
# Define the control using a random forest
control_rf <- rfeControl(functions=rfFuncs, method="cv", number=10) #This function generates a control object that can 
# be used to specify the details of the feature selection algorithms used in this package.

# Recursive feature elimination (RFE) algorithm (Backwards feature selection)(NUM)
results_rfe <- rfe(fc_data_2_salesCateg[,(1:10)], fc_data_2_salesCateg[,11], sizes=c(1:11), rfeControl=control_rf)
print(results_rfe)

# list the chosen features
predictors(results_rfe)
plot(results_rfe, type=c("g", "o"))

#### Calculate variable importance using random forest library  to compare

names(fc_data_4)[14]= "brand_kinder_cola"
names(fc_data_4)[15]= "brand_lemon_boost"
names(fc_data_4)[16]= "brand_orange_power"

library(randomForest)
rf_model = randomForest(sales_categories~., data=fc_data_2_salesCateg)
# Importance
importance(rf_model)
attr_importance = varImpPlot(rf_model, scale=F)
print(attr_importance)
attr_importance

plot(attr_importance, top =20, main='Variable Importance')


#...Dummy variables....

fc_data_2_dummy= catto_dummy(fc_data_2)
fc_data_2=fc_data_2_dummy
qcut = quantcut(fc_data_2$sales, q=5, labels= c("level_1", "level_2", "level_3", "level_4", "level_5"))
fc_data_2["sales_category"] = qcut
fc_data_4 = fc_data_2[,-2]


#partitioning
set.seed(100)
set <- createDataPartition(fc_data_2$sales, p = .70, list = FALSE)
training <- fc_data_2[ set,]
testing  <- fc_data_2[-set,]

# Convert the variable sales to categorical:

q=5
quant_training <- quantile(training$sales,   seq(0,1, length.out=q+1), TRUE)
dups <- duplicated(quant_training)
if(any(dups))
{
  warning('duplicated quantiles... not sure about this.')
}
sales_categ <- cut( training$sales, quant_training, include.lowest=TRUE,labels= c("level_1", "level_2", "level_3", "level_4", "level_5") )
training["sales"]=sales_categ

sales_categ <- cut( testing$sales, quant_training, include.lowest=TRUE,labels= c("level_1", "level_2", "level_3", "level_4", "level_5") )
testing["sales"]=sales_categ

# ****** Using mixed data set***************************************************************

#partitioning
set.seed(100)
set <- createDataPartition(mixed_df$sales, p = .70, list = FALSE)
training_mixed <- mixed_df[ set,c(-8,-9)]
testing_mixed  <- mixed_df[-set,c(-8,-9)]

#
q=5
quant_training <- quantile(training_mixed$sales,   seq(0,1, length.out=q+1), TRUE)
dups <- duplicated(quant_training)
if(any(dups))
{
  warning('duplicated quantiles... not sure about this.')
}
sales_categ_mixed<- cut( training_mixed$sales, quant_training, include.lowest=TRUE,labels= c("level_1", "level_2", "level_3", "level_4", "level_5") )
training_mixed["sales"]=sales_categ_mixed

sales_categ_mixed <- cut( testing_mixed$sales, quant_training, include.lowest=TRUE,labels= c("level_1", "level_2", "level_3", "level_4", "level_5") )
testing_mixed["sales"]=sales_categ_mixed


#*****************************************************************************


# NOTE: fc_data_2 is a data set with all numeric variables, use data frames -> training, testing

#****training set with feature selection

training_fs= training[,c(-3,-4,-9,-10,-11,-12,-13)]
testing_fs = testing[,c(-3,-4,-9,-10,-11,-12,-13)]

training_fs_test= training_fs[,c(-15,-16,-17,-18)]
testing_fs_test= testing_fs[,c(-15,-16,-17,-18)]

# ****************Clasification-----Tune using Caret Package*******

# Random Search

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random", classProbs = TRUE)
set.seed(101)
metric <- "Accuracy"
#mtry <- sqrt(ncol(training[,1:8])) # Default for classification
mtry <- sqrt(ncol(training_mixed)) # Default for classification
# RF Model***
random_RF <- train(sales~., data=training, method="rf", metric=metric, tuneLength=5, trControl=control)
print(random_RF)
plot(random_RF)
# estimate variable importance
importance <- varImp(random_RF, scale=FALSE)
plot(importance)

# Mixed data set without transformation
random_RF_m <- train(sales_category~., data=training_mixed, method="rf", metric=metric, tuneLength=5, trControl=control)
print(random_RF_m)
plot(random_RF)
imp <- varImp(random_RF_m, scale=FALSE)
plot(imp)



# SVM Model***
set.seed(103)
svm_model <- train(sales~ ., data = training_fs, method = "svmRadial", trControl = control, preProc = c("center", "scale"),tuneLength = 5,metric = "Accuracy")
svm_model
summary(svm_model)

# NN Model***
set.seed(108)
nn_model <- train(sales~ ., data = training_fs, method = "mlpML", trControl = control, preProc = c("center", "scale"),tuneLength = 5,metric = "Accuracy")
nn_model
set.seed(109)
nn_model_all <- train(sales~ ., data = training, method = "mlpML", trControl = control, preProc = c("center", "scale"),tuneLength = 5,metric = "Accuracy")
nn_model_all

# RRF Model***
# set.seed(109)
# rrf_model <- train(sales~ ., data = training, method = "RRF", trControl = control, preProc = c("center", "scale"),tuneLength = 5,metric = "Accuracy")
# rrf_model

#*********Performance differences

resamp<- resamples(list(RF= random_RF, SVM= svm_model, NN= nn_model))
resamp
summary(resamp)

# Visualization of the 4 models

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamp, layout = c(3, 1))




#*********Predict
sales_predict<- predict(random_RF, testing) # 
sales_predict
# Compare predicted outcome and true outcome

confusionMatrix(sales_predict,testing$sales)

#**** ROC*****

library(pROC)
roc_obj <-multiclass.roc(response = testing$sales, predictor = sales_predict)
auc(roc_obj)
roc_obj$auc
library(multiROC)
plot_roc_data(roc_obj)

# ************ Clustering *****************************
# Create a df containing only the highest sales:

fc_data_high_sales<-fc_data_2_salesCateg1[fc_data_2_salesCateg1$sales_categories==c("level_4", "level_5"),-9]
summary(fc_data_high_sales)
str(fc_data_high_sales)


# Clustering Tests-----

fc_df <- data.frame(fc_data_2$sales , fc_data_2$Season) 
ggplot(fc_df, aes(x = fc_data_2$sales, y = fc_data_2$Season )) +
  geom_point()

# Elbow methods 
library(factoextra)   
fviz_nbclust(fc_df, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

# Test diferent values of k

k1 = kmeans(fc_data_2, centers = 2, nstart = 10)
k2 = kmeans(fc_data_2, centers = 3, nstart = 10)
k3 = kmeans(fc_data_2, centers = 4, nstart = 10)
k4 = kmeans(fc_data_2, centers = 5, nstart = 10)
k5 = kmeans(fc_data_2, centers = 6, nstart = 10)

#plots to compare

p1 = fviz_cluster(k1, geom = "point", fc_data_2) + ggtitle("k=2")
p2 = fviz_cluster(k2, geom = "point", fc_data_2) + ggtitle("k=3")
p3 = fviz_cluster(k3, geom = "point", fc_data_2) + ggtitle("k=4")
p4 = fviz_cluster(k4, geom = "point", fc_data_2) + ggtitle("k=5")
p5 = fviz_cluster(k5, geom = "point", fc_data_2) + ggtitle("k=6")

#library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5, nrow=2)

# Using kmeans with k=3
cluster_3 <- kmeans(fc_data_2,centers = 3,nstart = 10)
cluster_3$cluster <- as.factor(cluster_3$cluster)
cluster_3
str(cluster_3)

ggplot(fc_data_2, aes(W1,W44,color =cluster_3$cluster)) +geom_point()


# numeric values only

fc_data_3 = fc_data_1[,c(-1,-8)]
str(fc_data_3)


# Another method PAM for mixed datasets***********************

library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne) #install package

#' Compute Gower distance
gower_dist <- daisy(fc_data_high_sales, metric = "gower")

gower_mat <- as.matrix(gower_dist)
summary(gower_mat)

#' Print most similar records
fc_data_3[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
#' Print most dissimilar records
fc_data_3[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]


#The silhouette figure helps us identify the best option(s).

# Siluethe figure to identify the best k

sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width}

plot(1:8, sil_width,
      xlab = "Number of clusters", ylab = "Silhouette Width")
lines(1:8, sil_width)

# Applying PAM using k=7

k <- 7
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- fc_data_high_sales %>% mutate(cluster = pam_fit$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results$the_summary


# Visualization

sne_obj = Rtsne(gower_dist, is_distance = TRUE) 
tsne_data = sne_obj$Y %>%data.frame() %>%setNames(c("X", "Y")) %>% mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))
