# Now we are doing analysis on amazon review 
# sentimental analysis

#import library which we need 
library(rvest)
library(xml2)
library(magrittr)
library(aurl)
library(wordcloud)
library(tm)

#
aurl <- "https://www.amazon.in/product-reviews/B07XVLH744/ref=cm_cr_getr_d_paging_btm_prev_1?pageNumber"
amazon_reviews <- NULL
for (i in 1:30){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
View(amazon_reviews)
str(amazon_reviews)
head(amazon_reviews)
# summary of data 
summary(amazon_reviews)
#save a file to local
write.table(amazon_reviews,"apple.txt",row.names = F)
getwd()

View(apple)
attach(apple)
#convert to corpus
corpus_review=Corpus(VectorSource(apple$V1))

# text preprocessing
corpus_review=tm_map(corpus_review, tolower)

corpus_review=tm_map(corpus_review, removePunctuation)
#Remove stopwords
corpus_review=tm_map(corpus_review, removeWords, stopwords("english"))
# Remove context specific stop words
corpus_review=tm_map(corpus_review, removeWords,c("also", "get","like", "company", "made", "can", "im", "dress", "just", "i"))

## Stem document
corpus_review=tm_map(corpus_review, stemDocument)
##Viewing the corpus content
corpus_review[[8]][1]

# create tdm and dtm
review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review)

# Convert TDM to matrix
review_m <- as.matrix(review_tdm)
# Sum rows and frequency data frame
review_term_freq <- rowSums(review_m)
# Sort term_frequency in descending order
review_term_freq <- sort(review_term_freq, decreasing = T)
# View the top 10 most common words
review_term_freq[1:10]

# Plot a barchart of the 20 most common words
barplot(review_term_freq[1:20], col = "steel blue", las = 2)

# word cloud
review_word_freq <- data.frame(term = names(review_term_freq),
                               num = review_term_freq)
# Create a wordcloud for the values in word_freqs
wordcloud(review_word_freq$term, review_word_freq$num,
          max.words = 100, colors = "red")


# Here we extract reviews from amazon
# Then convert into corpus
# Apply some method punctuation lower case upper case and common words freq stemmig on it
# Make tdm matrix
# plot some graph on it bar plot
# Plot a word cloud on it


