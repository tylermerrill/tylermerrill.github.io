# Load required library
library(pdftools)

# Set the directory containing PDF files
pdf_folder <- "/Users/rahulthakar/Downloads/Consolidated Pdf Docs/"  # Update with the correct path
output_folder <- "/Users/rahulthakar/Downloads/Consilidated Txt Files/"  # Folder to save the .txt files

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Get list of all PDF files in the folder
pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)

# Loop through each PDF file
for (pdf_file in pdf_files) {
  # Extract the text from the PDF
  pdf_text_data <- pdf_text(pdf_file)
  # Collapse the text pages into one string
  full_text <- paste(pdf_text_data, collapse = "\n")
  # Get the base file name (without path and extension)
  base_name <- tools::file_path_sans_ext(basename(pdf_file))
  # Define the output text file path
  output_file <- file.path(output_folder, paste0(base_name, ".txt"))
  # Write the text to the .txt file
  writeLines(full_text, con = output_file)
  cat("Converted:", pdf_file, "to", output_file, "\n")  # Print progress
}

# Data Prep:
library(pdftools)
require(tm) 
require(readtext) 
library(tidytext)
require(quanteda)

# Clear work environment
rm(list=ls())

# Import all text files.
txts <- readtext("/Users/hibbobjibjob/Documents/classes/Methods/group/txts/*.txt")

# Examine data.
head(txts)
head(txts$doc_id)
head(txts$text)[1]

# Check encoding.
Encoding(txts$text)

# Convert to UTF-8.
txts$text<- iconv(txts$text, from = "UTF-8", to = "UTF-8", sub = "")

# Remove encoded text and non-ascii characters as this can mess up further work.
txts$text <-gsub("[^[:print:]]", "", txts$text) # Remove encoded text.
txts$text <- gsub("[^\x20-\x7E]", "", txts$text) # Remove non ascii characters. 

# Convert txts dataframe to simple corpus.
txts_corpus <- Corpus(VectorSource(txts$text))

# Examine corpus.
summary(txts_corpus)

### Remove punctuation ###
txts_corpus_clean <- tm_map(txts_corpus, removePunctuation)

head(txts_corpus[[1]]$content) # Examine original text.
head(txts_corpus_clean[[1]]$content) # Examine clean text.

### Remove numbers ###
txts_corpus_clean <- tm_map(txts_corpus_clean, removeNumbers) # Notice how we replace the original corpus with the cleaned corpus

head(txts_corpus[[1]]$content) # Examine original text.
head(txts_corpus_clean[[1]]$content) # Examine clean text.

### Convert text to lower case ###
txts_corpus_clean <- tm_map(txts_corpus_clean, content_transformer(tolower))

head(txts_corpus[[1]]$content) # Examine original text.
head(txts_corpus_clean[[1]]$content) # Examine clean text.

### Remove stop words ###
txts_corpus_clean <- tm_map(txts_corpus_clean, removeWords, stopwords())

head(txts_corpus[[1]]$content) # Examine original text.
head(txts_corpus_clean[[1]]$content) # Examine clean text.

### Stem text ###
txts_corpus_clean <- tm_map(txts_corpus_clean, stemDocument, language = "english")  

head(txts_corpus[[1]]$content) # Examine original text.
head(txts_corpus_clean[[1]]$content) # Examine clean text.

### Remove white-space ###
txts_corpus_clean <- tm_map(txts_corpus_clean, stripWhitespace)  

head(txts_corpus[[1]]$content) # Examine original text.
head(txts_corpus_clean[[1]]$content) # Examine clean text.

# Let's make a DTM (Document-Term Matrix) too:

# Create DTM.
emma_dtm <- DocumentTermMatrix(txts_corpus_clean) # if working from a data frame, needs to be converted to a corpus first (see above)

# Examine dtm.
inspect(emma_dtm) 

# Remove sparse terms.
# We only only trying to keep terms
# that are shared across our EMMA corpus
emma_dtm <- removeSparseTerms(emma_dtm, 0.1)

# Examine dtm.
inspect(emma_dtm) 

# Convert dtm to tibble:
# Mostly will use this w/ tidyverse
# b/c it's a tidy df
emma_dta <- tidy(emma_dtm)
View(emma_dta)
# as a df for saving
emma_df <- as.data.frame(emma_dta)

# Save dataframe to csv
write.csv(emma_df, "/Users/hibbobjibjob/Documents/classes/Methods/group/csv/EMMA_df", row.names = FALSE)

# Some initial tokenization:
# Let us also construct a tokens object,
# w/ a new corpus object.
# NOTE: Sparse terms still here so less lean!

emma_corp <- corpus(txts_corpus_clean)
print(emma_corp)
summary(emma_corp, 10)

# Make a token frame:
toks_emma <- tokens(emma_corp)
print(toks_emma)

# Create dfm (Document-Feature matrix) object from token frame.
# Slightly different from dtm but useful for
# quanteda analysis. Can be used w/ tidyverse too...
dfmat_emma <- dfm(toks_emma)
View(dfmat_emma)

# Redux for quick loading:
txts <- readtext("/Users/hibbobjibjob/Documents/classes/Methods/group/txts/*.txt")
txts$text<- iconv(txts$text, from = "UTF-8", to = "UTF-8", sub = "")
txts$text <-gsub("[^[:print:]]", "", txts$text) # Remove encoded text.
txts$text <- gsub("[^\x20-\x7E]", "", txts$text) # Remove non ascii characters. 
txts_corpus <- Corpus(VectorSource(txts$text))
txts_corpus_clean <- tm_map(txts_corpus, removePunctuation)
txts_corpus_clean <- tm_map(txts_corpus_clean, content_transformer(tolower))
txts_corpus_clean <- tm_map(txts_corpus_clean, removeWords, stopwords())
txts_corpus_clean <- tm_map(txts_corpus_clean, stemDocument, language = "english")  
txts_corpus_clean <- tm_map(txts_corpus_clean, stripWhitespace)  

emma_dtm <- DocumentTermMatrix(txts_corpus_clean)
emma_dtm <- removeSparseTerms(emma_dtm, 0.1)
emma_dta <- tidy(emma_dtm)

emma_corp <- corpus(txts_corpus_clean)
toks_emma <- tokens(emma_corp)
dfmat_emma <- dfm(toks_emma)

# Unstemmed DTM for dictionary approaches:
txts2 <- readtext("/Users/hibbobjibjob/Documents/classes/Methods/group/txts/*.txt")
txts2$text<- iconv(txts2$text, from = "UTF-8", to = "UTF-8", sub = "")
txts2$text <-gsub("[^[:print:]]", "", txts2$text) 
txts2$text <- gsub("[^\x20-\x7E]", "", txts2$text)
txts2_corpus <- Corpus(VectorSource(txts2$text))
txts2_corpus_clean <- tm_map(txts2_corpus, removePunctuation)
txts2_corpus_clean <- tm_map(txts2_corpus_clean, removeNumbers)
txts2_corpus_clean <- tm_map(txts2_corpus_clean, content_transformer(tolower))
txts2_corpus_clean <- tm_map(txts2_corpus_clean, removeWords, stopwords())
txts2_corpus_clean <- tm_map(txts2_corpus_clean, stripWhitespace)  

emma2_corp <- corpus(txts2_corpus_clean)
toks2_emma <- tokens(emma2_corp)
emma2 <- dfm(toks2_emma)