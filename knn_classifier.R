#---------------------------------------------
# K-Nearest Neighbours Classifier
#---------------------------------------------

library(class)
source("preprocessor.R")

# Run preprocessor (param: random seed, train size, sparsity)
preprocess(100, .8, 0.999)


# Predict class labels
time.start <- Sys.time()
knn.pred <- knn(dtm.train.df[,names(dtm.train.df) != "category"],
                dtm.test.df[, names(dtm.test.df) != "category"], 
                dtm.train.df$category, k=1) # Test different k
(time.end <- Sys.time() - time.start)

# Create data frame of predicted and actual class labels
knn.pred.df <- data.frame(name=prod.test$name)
knn.pred.df <- cbind(knn.pred.df, actual=as.character(dtm.test.df$category), stringsAsFactors = FALSE)
knn.pred.df <- cbind(knn.pred.df, predict=as.character(knn.pred), stringsAsFactors = FALSE)

# Create data frame of misclassified objects
knn.misclass <- knn.pred.df[which(knn.pred.df$predict != knn.pred.df$actual),]

# Show confusion matrix
table(knn.pred.df$actual, factor(knn.pred), dnn=c("Actual","Predicted"))

# Calculate accuracy
nrow(knn.pred.df[which(knn.pred.df$predict == knn.pred.df$actual),]) / nrow(knn.pred.df)
