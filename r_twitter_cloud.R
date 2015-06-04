require(plyr)
require(png)
require(stringr)
require(tm)

library(wordcloud)
library(png)
library(stringr)
library(tm)

clean.text <- function(some_txt)
{
  some_txt = gsub("&amp", "", some_txt)
  
  some_txt = gsub("(RT|via)((?:\b\\W*@\\w+)+)", "", some_txt)
  
  some_txt = gsub("@\\w+", "", some_txt)
  
  some_txt = gsub("[[:punct:]]", "", some_txt)
  
  some_txt = gsub("[[:digit:]]", "", some_txt)
  
  some_txt = gsub("http\\w+", "", some_txt)
  
  some_txt = gsub("[ t]{2,}", "", some_txt)
  
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  # define "tolower error handling" function
  
  try.tolower = function(x)
    
  {
    
    y = NA
    
    try_error = tryCatch(tolower(x), error=function(e) e)
    
    if (!inherits(try_error, "error"))
      
      y = tolower(x)
    
    return(y)
    
  }
  
  some_txt = sapply(some_txt, try.tolower)
  
  some_txt = some_txt[some_txt != ""]
  
  names(some_txt) = NULL
  
  return(some_txt)
  
}

tweets = searchTwitter("#innovazione", n=3500, cainfo="cacert.pem") #chiave di ricerca

tweets.text = laply(tweets,function(t)t$getText())

clean_text = clean.text(tweets.text)

tweet_corpus = Corpus(VectorSource(clean_text))

tdm = TermDocumentMatrix(tweet_corpus, control = list(removePunctuation = TRUE,stopwords = c("machine", "learning", stopwords("english")), removeNumbers = TRUE, tolower = TRUE))

m = as.matrix(tdm) #we define tdm as matrix

word_freqs = sort(rowSums(m), decreasing=TRUE) #now we get the word orders in decreasing order

dm = data.frame(word=names(word_freqs), freq=word_freqs) #we create our data set

#wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2")) #and we visualize our data
wordcloud(dm$word,dm$freq, scale=c(8,.2),min.freq=3, max.words=Inf, random.order=FALSE, rot.per=.15)

#tosave csv uncomment
write.table(twListToDF(tweets), file= "23042015_#innovazione_3500tweets.csv",sep=";")
