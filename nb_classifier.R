#---------------------------------------------
# Naive Bayes Classifier
#---------------------------------------------

install.packages("e1071")
library(e1071)
source("preprocessor.R")

# Run preprocessor (param: randomize, sparsity)
preprocess(FALSE, 0.999)

start.time <- Sys.time()

# Create model
nb.model <- naiveBayes(category ~ ., dtm.train.df)

# Predict class labels
nb.pred <- predict(nb.model, dtm.test.df[,names(dtm.test.df) != "category"])

# Create data frame of predicted and actual class labels
nb.pred.df <- data.frame(name=prod.test$name)
nb.pred.df <- cbind(nb.pred.df, actual=as.character(dtm.test.df$category), stringsAsFactors = FALSE)
nb.pred.df <- cbind(nb.pred.df, predict=as.character(nb.pred), stringsAsFactors = FALSE)

# Create data frame of misclassified objects
nb.misclass <- nb.pred.df[which(nb.pred.df$predict != nb.pred.df$actual),]

# Show confusion matrix
table(nb.pred.df$actual, factor(nb.pred), dnn=c("Actual","Predicted"))

# Calculate accuracy
nrow(nb.pred.df[which(nb.pred.df$predict == nb.pred.df$actual),]) / nrow(nb.pred.df)

end.time <- Sys.time()
end.time - start.time