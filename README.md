# Product Categorization

## Description
This is an implementation of an approach to categorize e-commerce products using text mining and machine learning.

## Usage
1. Install R from https://cran.r-project.org
2. Install RStudio from https://www.rstudio.com (optional but recommended)
3. Uncomment "install.packages" in each file to install required packages and replace comment once done
4. Source files in the following order:
- preprocessor.R
- dt_classifier.R
- nb_classifier.R
- knn_classifier.R
- query.R
5. Enter a new product name for categorization

Note: due to the long execution time of training and testing the models, it may be preferable to simply load the already trained models from ".RData" into the environment.

## Data Set
flipkart.csv: 20,000 Flipkart.com product records with 15 attributes [1]

## References
[1] PromptCloud.: Flipkart Products, Version 1. Retrieved February 2018 from https://www.kaggle.com/PromptCloudHQ/flipkart-products, 2017.
