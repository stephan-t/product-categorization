#### Preprocessor ####

# install.packages("tm")
# install.packages("SnowballC")
# install.packages("RWeka")
library(tm)        # Text mining
library(SnowballC) # Stemming
# library(RWeka)   # n-grams


# Preprocess data set or query
preprocess <- function(sparsity = NULL, query = NULL) {
  # time.start <- Sys.time()
  
  if (is.null(query)) {
    # Import data
    data <- read.csv("data/flipkart.csv")
    
    # Create data frame with name and category columns
    data <- data[, 4:5]
    colnames(data) <- c("name","category")
    
    # Remove duplicates
    data <- unique(data)
    
    
    #### Text Processing ####
    
    # Extract top category
    data$category <- gsub('(\\["| >>.*)', "", data$category)
    data$category <- as.factor(data$category)
    
    # Remove unknown categories
    data <- data[(grep('.*"]', data$category, invert = TRUE)),]
    rownames(data) <- 1:nrow(data)
  }
  
  # Create corpus from data set or query
  if (is.null(query)) {
    data.corp <- VCorpus(VectorSource(data$name))
  } else {
    data.corp <- VCorpus(VectorSource(query))
  }
  
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
  data.corp <- tm_map(data.corp, content_transformer(uniqueWords))
  
  
  #### Feature Extraction ####

  # Create document term matrix with 2-gram features
  # BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  # data.dtm <- DocumentTermMatrix(data.corp, control = list(tokenize = BigramTokenizer))
 
  # Create document term matrix with 1-gram features
  data.dtm <- DocumentTermMatrix(data.corp)
  
  # Reduce size of DTM by removing less frequent terms
  if (is.null(query)) {
    data.dtm <- removeSparseTerms(data.dtm, sparsity)
  }
  
  # Convert DTM to data frame
  data.df <- data.frame(as.matrix(data.dtm))
  if (is.null(query)) {
    # Append product names and classes
    data.df <- cbind(name = data$name, data.df)
    data.df <- cbind(data.df, category = data$category)
  }
  
  # Change term columns to binary factor
  if (is.null(query)) {
    data.df[, -c(1, ncol(data.df))] <- lapply(data.df[, -c(1, ncol(data.df))], factor)
  } else {
    data.df[, 1:ncol(data.df)] <- lapply(data.df[, 1:ncol(data.df)] , factor, levels = c("0","1"))
  }

  # Update class label factor levels
  if (is.null(query)) {
    data.df$category <- factor(data.df$category)
  }
  
  # print(time.end <- Sys.time() - time.start)
  return(data.df)
}
