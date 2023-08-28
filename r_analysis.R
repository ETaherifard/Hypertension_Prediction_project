library(readxl)
library(randomForest)
library(pROC)
library(devtools)
library(caret)
library(Matrix)
library(rpart)
library(glmnet)
library(e1071)
library(MASS)

#/////////////////////////////
data<- read_excel("datadt_t.xlsx")

num_na <- sum(is.na(data))
#"Sum null values: "
print(num_na)

#delete null values
data <- na.omit(data)



data$Gender<-as.factor(data$Gender)
data$Ethnicity<-as.factor(data$Ethnicity )
data$Maritalstatus<-as.factor(data$Maritalstatus)
data$Education<-as.factor(data$Education )
data$CigaretteTobacco_use<-as.factor(data$CigaretteTobacco_use)
data$Diabetesdisease<-as.factor(data$Diabetesdisease )
data$Heartdisease<-as.factor(data$Heartdisease)
data$Chronickidneydisease<-as.factor(data$Chronickidneydisease )
data$Strokedisease<-as.factor(data$Strokedisease )
data$PictogramAge15_cat<-as.factor(data$PictogramAge15_cat)
data$PictogramAge30_cat<-as.factor(data$PictogramAge30_cat )


#Descriptive statistics for Table 1 
str (data)
mean (data$Age)
sd (data$Age)
meanAge_sub <- aggregate (data$Age, by= list (data$Hypertension), FUN= mean)
meanAge_sub
sdAge_sub <- aggregate (data$Age, by= list (data$Hypertension), FUN= sd)
sdAge_sub
t.test (Age~ Hypertension, data= data)

mean (data$ALP)
sd (data$ALP)
meanALP_sub <- aggregate (data$ALP, by= list (data$Hypertension), FUN= mean)
meanALP_sub
sdALP_sub <- aggregate (data$ALP, by= list (data$Hypertension), FUN= sd)
sdALP_sub
t.test (ALP~ Hypertension, data= data)

mean (data$AST)
sd (data$AST)
meanAST_sub <- aggregate (data$AST, by= list (data$Hypertension), FUN= mean)
meanAST_sub
sdAST_sub <- aggregate (data$AST, by= list (data$Hypertension), FUN= sd)
sdAST_sub
t.test (AST~ Hypertension, data= data)

mean (data$ALT)
sd (data$ALT)
meanALT_sub <- aggregate (data$ALT, by= list (data$Hypertension), FUN= mean)
meanALT_sub
sdALT_sub <- aggregate (data$ALT, by= list (data$Hypertension), FUN= sd)
sdALT_sub
t.test (ALT~ Hypertension, data= data)

mean (data$Cholesterol)
sd (data$Cholesterol)
meanCholesterol_sub <- aggregate (data$Cholesterol, by= list (data$Hypertension), FUN= mean)
meanCholesterol_sub
sdCholesterol_sub <- aggregate (data$Cholesterol, by= list (data$Hypertension), FUN= sd)
sdCholesterol_sub
t.test (Cholesterol~ Hypertension, data= data)

mean (data$HDL)
sd (data$HDL)
meanHDL_sub <- aggregate (data$HDL, by= list (data$Hypertension), FUN= mean)
meanHDL_sub
sdHDL_sub <- aggregate (data$HDL, by= list (data$Hypertension), FUN= sd)
sdHDL_sub
t.test (HDL~ Hypertension, data= data)

mean (data$LDL)
sd (data$LDL)
meanLDL_sub <- aggregate (data$LDL, by= list (data$Hypertension), FUN= mean)
meanLDL_sub
sdLDL_sub <- aggregate (data$LDL, by= list (data$Hypertension), FUN= sd)
sdLDL_sub
t.test (LDL~ Hypertension, data= data)

mean (data$Triglyceride)
sd (data$Triglyceride)
meanTriglyceride_sub <- aggregate (data$Triglyceride, by= list (data$Hypertension), FUN= mean)
meanTriglyceride_sub
sdTriglyceride_sub <- aggregate (data$Triglyceride, by= list (data$Hypertension), FUN= sd)
sdTriglyceride_sub
t.test (Triglyceride~ Hypertension, data= data)

mean (data$FBS)
sd (data$FBS)
meanFBS_sub <- aggregate (data$FBS, by= list (data$Hypertension), FUN= mean)
meanFBS_sub
sdFBS_sub <- aggregate (data$FBS, by= list (data$Hypertension), FUN= sd)
sdFBS_sub
t.test (FBS~ Hypertension, data= data)

table(data$Gender, data$Hypertension)
chisq.test(table(data$Gender, data$Hypertension))

table(data$Ethnicity, data$Hypertension)
chisq.test(table(data$Ethnicity, data$Hypertension))

table(data$Maritalstatus, data$Hypertension)
chisq.test(table(data$Maritalstatus, data$Hypertension))

table(data$Education, data$Hypertension)
chisq.test(table(data$Education, data$Hypertension))

table(data$Physical_Activity, data$Hypertension)
chisq.test(table(data$Physical_Activity, data$Hypertension))

table(data$Socioeconomic_status, data$Hypertension)
chisq.test(table(data$Socioeconomic_status, data$Hypertension))

table(data$CigaretteTobacco_use, data$Hypertension)
chisq.test(table(data$CigaretteTobacco_use, data$Hypertension))

table(data$Diabetesdisease, data$Hypertension)
chisq.test(table(data$Diabetesdisease, data$Hypertension))

table(data$Heartdisease, data$Hypertension)
chisq.test(table(data$Heartdisease, data$Hypertension))

table(data$Socioeconomic_status, data$Hypertension)
chisq.test(table(data$Socioeconomic_status, data$Hypertension))

table(data$Chronickidneydisease, data$Hypertension)
chisq.test(table(data$Chronickidneydisease, data$Hypertension))

table(data$Strokedisease, data$Hypertension)
chisq.test(table(data$Strokedisease, data$Hypertension))

table(data$Multimorbidity, data$Hypertension)
chisq.test(table(data$Multimorbidity, data$Hypertension))

table(data$Abdominal_obesity, data$Hypertension)
chisq.test(table(data$Abdominal_obesity, data$Hypertension))

table(data$PictogramAge15_cat, data$Hypertension)
chisq.test(table(data$PictogramAge15_cat, data$Hypertension))

table(data$PictogramAge30_cat, data$Hypertension)
chisq.test(table(data$PictogramAge30_cat, data$Hypertension))

#
barchart(data$Diabetesdisease,main = "Diabetesdisease")
barchart(data$Heartdisease,main = "Heartdisease")
barchart(data$CigaretteTobacco_use,main = "CigaretteTobacco_use")
barchart(data$Ethnicity,main = "Ethnicity")
barchart(data$Education,main = "Education")
barchart(data$Gender,main = "Gender")
hist(data$Age,main = "Age")
hist(data$FBS,main = "FBS")
hist(data$HDL,main = "HDL")
#hist(data$Hypertension, main = "Histogram of Array", xlab = "Values", ylab = "Frequency", col = "lightblue")

# Split data into train and test sets (70% train, 30% test)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

train_data<-as.data.frame(train_data)
test_data<-as.data.frame(test_data)
# Define the target variable column index in the data frame
target_column <- "Hypertension"
target_index <- which(colnames(train_data) == target_column)
train_data$Hypertension <- as.factor(train_data$Hypertension)
# Train the random forest model
model_rf <- randomForest(train_data[, -target_index], train_data[, target_index])
varImpPlot(model_rf)


# Make predictions on the test data
predictions <- predict(model_rf, test_data[, -target_index])

# Calculate accuracy
accuracy <- sum(predictions == test_data[, target_index]) / nrow(test_data) * 100
print(paste("Accuracy:", accuracy, "%"))
# Calculate AUC
roc_obj <- roc(response = predictions, predictor = test_data[, target_index])
auc <- auc(roc_obj)
print(auc)
plot(roc_obj, main = "RF-ROC Curve", print.auc = TRUE, auc.polygon = TRUE)

# Calculate recall
recall <- confusionMatrix(predictions,  as.factor(test_data[, target_index]))$byClass['Recall']
print(recall)



# Calculate precision
precision <- confusionMatrix(predictions,  as.factor(test_data[, target_index]))$byClass['Precision']
print(precision)

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)


#///////////////////////// logestic regression ////////////////////////////////////////


lr_model <- glm(train_data[, target_index] ~.,data =train_data[, -target_index], family="binomial" )
# Make predictions on the test data
importance <- varImp(lr_model)
print(importance)
ggplot(importance, aes(x = reorder(rownames(importance), Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  theme_minimal() +
  labs(x = "Feature", y = "Importance Score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.margin = margin(1, 1, 1, 3, "cm"))


predictions_lr <- predict(lr_model, newdata =test_data[, -target_index])
mat <- ifelse(predictions_lr > 0.5, 1, 0)


accuracy_lr <- sum(mat == test_data[, target_index]) / nrow(test_data) * 100
print(paste("Accuracy:", accuracy_lr, "%"))
# Calculate AUC
roc_obj <- roc(response = mat, predictor = test_data[, target_index])
auc_lr <- auc(roc_obj)
print(auc_lr)

plot(roc_obj, main = "LR-ROC Curve", print.auc = TRUE, auc.polygon = TRUE)
# Calculate recall
recall_lr <- confusionMatrix(as.factor(mat),  as.factor(test_data[, target_index]))$byClass['Recall']
print(recall_lr)



# Calculate precision
precision_lr <- confusionMatrix(as.factor(mat),  as.factor(test_data[, target_index]))$byClass['Precision']
print(precision_lr)

# Calculate F1 score
f1_score_lr <- 2 * (precision_lr * recall_lr) / (precision_lr + recall_lr)
print(f1_score_lr)



#///////////////////////////decision tree //////////////////////////

dt_model <- rpart(train_data[, target_index] ~.,data =train_data[, -target_index] )
# Make predictions on the test data


importance <- varImp(dt_model)
print(importance)
ggplot(importance, aes(x = reorder(rownames(importance), Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  theme_minimal() +
  labs(x = "Feature", y = "Importance Score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.margin = margin(1, 1, 1, 3, "cm"))


predictions_dt <- predict(dt_model, newdata =test_data[, -target_index],type = "class")



accuracy_dt <- sum(predictions_dt == test_data[, target_index]) / nrow(test_data) * 100
print(paste("Accuracy:", accuracy_dt, "%"))
# Calculate AUC
roc_obj <- roc(response = as.factor(predictions_dt), predictor = as.factor(test_data[, target_index] ))
auc_dt <- auc(roc_obj)
print(auc_dt)


# Calculate recall
recall_dt <- confusionMatrix(as.factor(predictions_dt),  as.factor(test_data[, target_index]))$byClass['Recall']
print(recall_dt)



# Calculate precision
precision_dt <- confusionMatrix(as.factor(predictions_dt),  as.factor(test_data[, target_index]))$byClass['Precision']
print(precision_dt)

# Calculate F1 score
f1_score_dt <- 2 * (precision_dt * recall_dt) / (precision_dt + recall_dt)
print(f1_score_dt)

#//////////////////////// SVM ////////////////////////////////////

svm_model <- svm(train_data[, target_index] ~.,data =train_data[, -target_index] )
# Make predictions on the test data


importance_values <- abs(svm_model$coefs)
print(importance_values)
#barplot(importance_values, names.arg = colnames(train_data[, -target_index]))




predictions_svm <- predict(svm_model, newdata =test_data[, -target_index],type = "class")



accuracy_svm <- sum(predictions_svm == test_data[, target_index]) / nrow(test_data) * 100
print(paste("Accuracy:", accuracy_svm, "%"))
# Calculate AUC
roc_obj <- roc(response = as.factor(predictions_svm), predictor = test_data[, target_index])
auc_svm <- auc(roc_obj)
print(auc_svm)


# Calculate recall
recall_svm <- confusionMatrix(as.factor(predictions_svm),  as.factor(test_data[, target_index]))$byClass['Recall']
print(recall_svm)



# Calculate precision
precision_svm <- confusionMatrix(as.factor(predictions_svm),  as.factor(test_data[, target_index]))$byClass['Precision']
print(precision_svm)

# Calculate F1 score
f1_score_svm <- 2 * (precision_svm * recall_svm) / (precision_svm + recall_svm)
print(f1_score_svm)



#/////////////////////////////// Gaussian Naive Bayes //////////////////////


gnb_model <- naiveBayes(train_data[, target_index] ~.,data =train_data[, -target_index] )
# Make predictions on the test data
#importance <- varImp(gnb_model)
ggplot(importance, aes(x = reorder(rownames(importance), Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  theme_minimal() +
  labs(x = "Feature", y = "Importance Score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.margin = margin(1, 1, 1, 3, "cm"))
predictions_gnb <- predict(gnb_model, newdata =test_data[, -target_index],type = "class")



accuracy_gnb <- sum(predictions_gnb == test_data[, target_index]) / nrow(test_data) * 100
print(paste("Accuracy:", accuracy_gnb, "%"))
# Calculate AUC
roc_obj <- roc(response = as.factor(predictions_gnb), predictor = test_data[, target_index] )
auc_gnb <- auc(roc_obj)
print(auc_gnb)


# Calculate recall
recall_gnb <- confusionMatrix(as.factor(predictions_gnb),  as.factor(test_data[, target_index]))$byClass['Recall']
print(recall_gnb)



# Calculate precision
precision_gnb <- confusionMatrix(as.factor(predictions_gnb),  as.factor(test_data[, target_index]))$byClass['Precision']
print(precision_gnb)

# Calculate F1 score
f1_score_gnb <- 2 * (precision_gnb * recall_gnb) / (precision_gnb + recall_gnb)
print(f1_score_gnb)

#/////////////////////////////////////// Linear Discriminant Analysis /////////////////


lda_model <- lda(train_data[, target_index] ~.,data =train_data[, -target_index] )
# Make predictions on the test data

predictions_lda <- predict(lda_model, newdata =test_data[, -target_index])$class



accuracy_lda <- sum(predictions_lda == test_data[, target_index]) / nrow(test_data) * 100
print(paste("Accuracy:", accuracy_lda, "%"))
# Calculate AUC
roc_obj <- roc(response = as.factor(predictions_lda), predictor = test_data[, target_index])
auc_lda <- auc(roc_obj)
print(auc_lda)


# Calculate recall
recall_lda <- confusionMatrix(as.factor(predictions_lda),  as.factor(test_data[, target_index]))$byClass['Recall']
print(recall_lda)



# Calculate precision
precision_lda <- confusionMatrix(as.factor(predictions_lda),  as.factor(test_data[, target_index]))$byClass['Precision']
print(precision_lda)

# Calculate F1 score
f1_score_lda <- 2 * (precision_lda * recall_lda) / (precision_lda + recall_lda)
print(f1_score_lda)





print("---------------------------- RESUALT/Compare models-------------------------------------")


res_table <- data.frame(
  model = c("RF", "LR", "DT","SVM","Gaussian Naive Bayes","Linear Discriminant Analysis"),
  accuracy = c(accuracy, accuracy_lr, accuracy_dt,accuracy_svm,accuracy_gnb,accuracy_lda),
  recall = c(recall, recall_lr, recall_dt,recall_svm,recall_gnb,recall_lda),
  precision = c(precision, precision_lr, precision_dt,precision_svm,precision_gnb,precision_lda),
  f1_score = c(f1_score, f1_score_lr, f1_score_dt,f1_score_svm,f1_score_gnb,f1_score_lda),
  auc = c(auc, auc_lr, auc_dt,auc_svm,auc_gnb,auc_lda)
 
)

# Print the table
print(res_table)


plot(roc_obj, main = "ROC Curve", print.auc = TRUE, auc.polygon = TRUE)



