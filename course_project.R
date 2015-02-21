#Libraries
library(caret); library(rattle); library(rpart); library(randomForest);
#url_train <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
#url_test <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

#read in data sets
pml_train <- read.csv(file = 'pml-training.csv',
                      na.strings = c('NA','#DIV/0!',''))
pml_test <- read.csv(file = 'pml-testing.csv',
                     na.strings = c('NA','#DIV/0!',''))

#remove first seven columns for building the model
for(i in c(8:ncol(pml_train)-1)) {
  pml_train[,i] = as.numeric(as.character(pml_train[,i]))
  pml_test[,i] = as.numeric(as.character(pml_test[,i]))
}


#build a list of all features not needed when running the models and the predictions
feature_i<- colnames(pml_train)
feature_i <- colnames(pml_train[colSums(is.na(pml_train)) == 0])
feature_i <- colnames(pml_train[colSums(is.na(pml_train)) == 0])
feature_i <- feature_i[-c(1:7)]

#set the seed for reproducibility
set.seed(12345)
#partition the data with 80% going into the training sample and 20% for the cross validation
i_train <- createDataPartition(y=pml_train$classe, p=0.80, list=FALSE)
#create the training samples and cross validation samples using only the needed features/columns
data_train <- pml_train[i_train,feature_i]
data_cross_val <- pml_train[-i_train,feature_i]

#verifiy that each set has the correct number of features/columns
dim(data_train)
dim(data_cross_val)


#create the model using random forest
model_fit <- randomForest(classe ~. , data=data_train)
#run the predictions
predict_rf <- predict(model_fit,data_cross_val, type = "class")

#run the confusion matrix to determine the accuracy
c_matrix_rf <- confusionMatrix(predict_rf, data_cross_val$classe)



#run our test set against our model
final_result <- length(colnames(pml_test[]))
colnames(pml_test)[final_result] <- 'classe'
test_model <- predict(model_fit,pml_test[,feature_i])
test_model

#write to text files for submission of assignment
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

#run write-to function
pml_write_files(test_model)