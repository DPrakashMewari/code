# Now we are doing Twitter sentimental analysis

# first we need authentication for twitter account

install.packages('ROAuth')
install.packages('twitteR')
library(ROAuth)
library(twitteR)

consumer_key <-"o7WjJyURwxVK5N3nzy10pbBNL"
consumer_secret <- "IGCypfT9aEANhWwKpJzp9uWGAFQNplcCvgNepOBFb7C8o6QXsM"
access_token<-"2328873745-ED17VX7oiAPQ0dKvnYlhEEPS1WG8rXtkNgP1spz"
access_secret <- "Ijs9EBZ2K7Y28l0TZZve4YbDF1OJPnNW7AIF6FCKHvxTM"

setup_twitter_oauth(consumer_key ,consumer_secret, access_token,  access_secret )

cred <- OAuthFactory$new(consumerKey='o7WjJyURwxVK5N3nzy10pbBNL', consumerSecret='IGCypfT9aEANhWwKpJzp9uWGAFQNplcCvgNepOBFb7C8o6QXsM',requestURL='https://api.twitter.com/oauth/request_token',accessURL='https://api.twitter.com/oauth/access_token',authURL='https://api.twitter.com/oauth/authorize')

cred$handshake(cainfo="cacert.pem")
#After this you will be redirected to a URL where you click on authorize app and get the passkey to be entered in RStudio


#Extracting tweets
cprakash.tweets = searchTwitter("@narendramodi", n=1500)  


# load the words 

#After downloading the hui.lui.pos and hui.lui.neg, mention below the location of the file
pos.words = scan(file.choose(), what='character', comment.char=';')
neg.words = scan(file.choose(), what='character', comment.char=';')



#adding words to positive and negative databases
pos.words=c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader')
neg.words = c(neg.words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')

# exracting textual part of the tweets
#sample=NULL #Initialising
#for (tweet in cprakash.tweets)
#  sample = c(sample,tweet$getText())




# create a data frame for it
df <- do.call("rbind", lapply(cprakash.tweets, as.data.frame))
?do.call

# now apply method for those text which collect and set by page row and font fot it
df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))


?sapply

# apply a score sentiment function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]',' ',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)
    sentence = gsub('\n','',sentence)
    
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp=sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1=c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new=lapply(list, `[[`, 1)
  pp1=score=lapply(list, `[[`, 2)
  nn1=score=lapply(list, `[[`, 3)
  
  scores.df = data.frame(score=score_new, text=sentences)
  positive.df = data.frame(Positive=pp1, text=sentences)
  negative.df = data.frame(Negative=nn1, text=sentences)
  
  list_df=list(scores.df, positive.df, negative.df)
  return(list_df)
}

# Now Clean the tweets
result = score.sentiment(df$text, pos.words, neg.words)


library(reshape)

# Now use a result and access three as a copy
test1=result[[1]]
test2=result[[2]]
test3=result[[3]]

View(test1)
#creating three different data frame for score postive and negative
#Removing text column from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL
View(test1)

#Storing the first row Containing the sentiment scores
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]
View(q1)
qq1=melt(q1, ,var='Score')
qq2=melt(q2, ,var='Positive')
qq3=melt(q3, ,var='Negative') 
View(qq1)
View(qq2)
?melt

#Creating data frame
table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[2]]$text, Score=qq2)
table3 = data.frame(Text=result[[3]]$text, Score=qq3)
attach(table1)
attach(table2)
attach(table3)

View(table1)
#Merging three data frames into one
table_final=data.frame(Text=table1, Score=table1$Text, Positive=table2$Score.value, Negative=table3$Score.value)
View(table_final)

#Making percentage columns

p=table_final$Positive/(table_final$Positive+table_final$Negative)
p[ is.nan(p) ] <- 0
table_final$Postive_percentage=p
# dividepos
n=table_final$Positive/(table_final$Positive+table_final$Negative)
n[ is.nan(n) ] <- 0
table_final$Neg_percent=n

View(table_final)

#creating a Histrogram
hist(table_final$Score, colour=rainbow(10))
hist(table_final$Positive, colour=rainbow(10))
hist(table_final$Negative, colour=rainbow(10))


# making a word cloud 
#install.packages("wordcloud")
library(wordcloud)

library(tm)

# making a document 
quake_corpus = Corpus(VectorSource(prakash_text))
#inspect some
inspect(quake_corpus[2])

#cleaning text using tmmap function and it helpful for also make matrix

quake_clean = tm_map(quake_corpus, removePunctuation)
quake_clean = tm_map(quake_clean, content_transformer(tolower))
quake_clean = tm_map(quake_clean, removeWords, stopwords("english"))
quake_clean = tm_map(quake_clean, removeNumbers)
quake_clean = tm_map(quake_clean, stripWhitespace)

#nowplot a word cloud
wordcloud(quake_clean, random.order=F,max.words=80, col=rainbow(50), scale=c(4,0.5))



# first i load some library data 
# then twitter authentication
# Extracting a tweets
# load the positive and negative words and if i want some addition i add there
#then we combine our tweets to data frame then apply function on it that compare positive and negative words on it
# Then we have split 3 variable postive negative sentence and make table each of it
# Then we convert our tweets to corpus
# some cleaning apply on it 
# Then we plot a word cloud on it


