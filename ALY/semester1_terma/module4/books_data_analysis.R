
# Import necessary libraries
library(tidyverse)

# Load the dataset
books <- read.csv("books.csv", stringsAsFactors = FALSE, skipNul = TRUE)

# Renaming columns
names(books) <- c('book_id', 'title', 'authors', 'average_rating', 'isbn', 'isbn_13',
                  'language', 'num_pages', 'ratings_count', 'text_reviews_count',
                  'publication_date', 'publisher')

# Correcting data types
books$publication_date <- as.Date(books$publication_date, format="%m/%d/%Y")

# Remove rows with NA in 'publication_date'
books <- na.omit(books, cols="publication_date")

# Calculate descriptive statistics for interesting variables
summary_stats <- summary(books[, c('average_rating', 'num_pages', 'ratings_count', 'text_reviews_count')])
