#### Build the model ####
join2<-read_csv(url('https://raw.githubusercontent.com/LMD-nat/follow-ups/main/ER_visits_set.csv'))
model <- join2
# join2 needs to have only numeric columns, so change all these values to some numeric placeholder
model$weekday <- ifelse(model$weekday == 'Monday', 1, 
                        ifelse(model$weekday == 'Tuesday', 2,
                               ifelse(model$weekday == 'Wednesday', 3, 
                                      ifelse(model$weekday == 'Thursday', 4,
                                             ifelse(model$weekday == 'Friday', 5,
                                                    ifelse(model$weekday == 'Saturday', 6,7))))))
model$return_day <- ifelse(model$return_day == 'Monday', 1, 
                        ifelse(model$return_day == 'Tuesday', 2,
                               ifelse(model$return_day == 'Wednesday', 3, 
                                      ifelse(model$return_day == 'Thursday', 4,
                                             ifelse(model$return_day == 'Friday', 5,
                                                    ifelse(model$return_day == 'Saturday', 6,7))))))
model$index_cat <- ifelse(model$index_cat == 'Morning', 2, 
                                                ifelse(model$index_cat == 'Afternoon', 3,
                                                       ifelse(model$index_cat == 'Evening', 4,1)))
model$return_cat <- ifelse(model$return_cat == 'Morning', 2, 
                          ifelse(model$return_cat == 'Afternoon', 3,
                                 ifelse(model$return_cat == 'Evening', 4,1)))

model$index_date <- as.numeric(model$index_arrival_date)
model$return_date <- as.numeric(model$arrival_date_rv)

library(dplyr)

df <- model %>% select(days_since_index, hours_btw_timestamps,
                       weekday, return_day, weekend_ER, index_cat, return_cat, ER_follow_up)

### ER followup is the predictor variable, a follow-up or not (1= yes, 0 = NO)
library(caret) # caret means Classification And REgression Training
set.seed(635479)

# the output is the response variable
df$output <- as.factor(df$ER_follow_up)
# we don't want the same variable twice, remove 
df <- df %>% select(-ER_follow_up)
# Pre-Processing of Predictors
preProcess <- c("center","scale")
# split the data into training and test sets, I like to split by .5, I think it's standard
i <- createDataPartition(y = df$output, times = 1, p = 0.5, list = FALSE)
# training set = people in i
training_set <- df[i,]
# training set = people not in i
test_set <- df[-i,]
# Use resampling to find the “best model” by choosing the values of the tuning parameters
# trControl will specify the resampling scheme
trControl <- trainControl(method = "repeatedcv", number = 100, repeats = 100)
# train specifies how cross-validation should be performed to find the best values of the tuning parameters
model <- train(output ~ ., method='knn', data = training_set, metric='Accuracy', preProcess = preProcess, trControl= trControl)
# this takes a few minutes to run!

# run this line to see info about the model, it's a k-nearest neighbors model
model

# k-Nearest Neighbors 
# 
# 315 samples
# 7 predictor
# 2 classes: '0', '1' 
# 
# Pre-processing: centered (7), scaled (7) 
# Resampling: Cross-Validated (100 fold, repeated 100 times) 
# Summary of sample sizes: 311, 312, 311, 313, 313, 312, ... 
# Resampling results across tuning parameters:
#   
#   k  Accuracy   Kappa    
# 5  0.9441000  0.8177740
# 7  0.9526333  0.8580917
# 9  0.9406167  0.8148899
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 7.

# ------------------------------------------------------

#### You'll probably see an warning message
#### This is almost always because some tuning parameter combination produced predictions that are constant for all samples. train() tries to compute 
### the R^2 and, since it needs a non-zero variance, it produces an NA for that statistic. I think!

### Training model done, check the output

# use the model to predict follow-ups from the test set
test_set$pred <- predict(model, test_set)
# the values the model predicted
test_set$factor_pred <- as.factor(test_set$pred)
# the values I chose as the "gold standard"
test_set$factor_truth <- as.factor(test_set$output)
# makes a confusion matrix
cm <- confusionMatrix(test_set$pred, test_set$output)

# running this line will output the model results AND the confusion matrix
cm


# Confusion Matrix and Statistics
# 
#            Reference (ME)
# Prediction (MODEL)   0   1
#                  0 227   0
#                  1  23  65
# 
# Accuracy : 0.927           
# 95% CI : (0.8925, 0.9532)
# No Information Rate : 0.7937          
# P-Value [Acc > NIR] : 5.908e-11       
# 
# Kappa : 0.8029          
# 
# Mcnemar's Test P-Value : 4.490e-06       
#                                           
#             Sensitivity : 0.9080          
#             Specificity : 1.0000          
#          Pos Pred Value : 1.0000          
#          Neg Pred Value : 0.7386          
#              Prevalence : 0.7937          
#          Detection Rate : 0.7206          
#    Detection Prevalence : 0.7206          
#       Balanced Accuracy : 0.9540          
#                                           
#        'Positive' Class : 0 


# for just the confusion matrix
accuracy <- cm$overall[1]
confusion_matrix <- cm$table
confusion_matrix

### Plot a ROC curve (receiver operating characteristic curve)
# we want the corner of the red line to be as close as possible to the top left corner
# the model performs pretty well, so that's what we should see!

library(pROC)
test_set$pred <- predict(model, test_set, probability=TRUE)
roc_curve = roc(test_set$output, predictor=factor(test_set$pred, ordered = TRUE))
plot(roc_curve, col="red", lwd=3, main = 'Model performance diagnosing follow-ups')

### Next steps: try different parameters, see how the sensitivity and specificity change!
### Reference guide: https://daviddalpiaz.github.io/r4sl/the-caret-package.html

