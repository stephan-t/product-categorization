#### Preprocessor ####

# install.packages("tm")
# install.packages("SnowballC")
library(tm)        # Text mining
library(SnowballC) # Stemming


# Preprocess function to run from each classifier
preprocess <- function(sparsity) {
  time.start <- Sys.time()
  
  # Import data
  data <- read.csv("data/flipkart.csv")
  
  # Create data frame with name and category columns
  data <- data[, 4:5]
  colnames(data) <- c("name","category")
  
  # Remove duplicates
  data <- unique(data)
  
  # Extract top category
  data$category <- gsub('(\\["| >>.*)', "", data$category)
  data$category <- as.factor(data$category)
  
  # Remove unknown categories
  data <- data[(grep('.*"]', data$category, invert = TRUE)),]
  rownames(data) <- 1:nrow(data)

  
  #### Text Processing ####
  
  # Create corpus
  data.corp <- VCorpus(VectorSource(data$name))
  
  # Function to count words once
  uniqueWords <- function(word) {
    return(paste(unique(strsplit(word, " ")[[1]]), collapse = ' '))
  }
  
  # Process text
  data.corp <- tm_map(data.corp, content_transformer(tolower))
  data.corp <- tm_map(data.corp, removePunctuation)
  data.corp <- tm_map(data.corp, removeNumbers)
  data.corp <- tm_map(data.corp, stripWhitespace)
  data.corp <- tm_map(data.corp, removeWords, stopwords("english"))
  data.corp <- tm_map(data.corp, stemDocument, language = "english")
  # data.corp <- tm_map(data.corp, removeWords, c("black","blue","white","yellow","men",
  #                                     "women","boy","girl"))
  data.corp <- tm_map(data.corp, content_transformer(uniqueWords))

  # Create document term matrix
  data.dtm <- DocumentTermMatrix(data.corp)
  
  # Reduce size of DTM by removing less frequent terms
  data.dtm <- removeSparseTerms(data.dtm, sparsity)
  
  # Convert DTM to data frame and append product names and class
  data.df <- data.frame(as.matrix(data.dtm))
  data.df <- cbind(name = data$name, data.df)
  data.df <- cbind(data.df, category = data$category)
  
  # Change term columns from numeric to factor
  data.df[, -c(1, ncol(data.df))] <- lapply(data.df[, -c(1, ncol(data.df))], factor)

  # Update class label factor levels
  data.df$category <- factor(data.df$category)
  
  print(time.end <- Sys.time() - time.start)
  return(data.df)
}