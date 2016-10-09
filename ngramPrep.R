library(stringi)
library(stringr)
library(tm)
library(RWeka)
library(SnowballC)
# Read the data set provided
blogs <- readLines('data/en_US/en_US.blogs.txt', encoding = 'UTF-8')
news <- readLines('data/en_US/en_US.news.txt',encoding = 'UTF-8')
twitter <- readLines('data/en_US/en_US.twitter.txt', encoding = 'UTF-8')

# Binomial sampling of the data to create the relevant files
sampling <- function(data, percent)
{
        return(data[as.logical(rbinom(length(data),1,percent))])
}
set.seed(1980)


sample.blogs   <- sampling(blogs, .02)
sample.news   <- sampling(news, .02)
sample.twitter   <- sampling(twitter, .02)

dir.create("sample", showWarnings = FALSE)

write(sample.blogs, "sample/sample.blogs.txt")
write(sample.news, "sample/sample.news.txt")
write(sample.twitter, "sample/sample.twitter.txt")

# Use the sampled data to create a Corpus
sample.corpus <- c(sample.blogs,sample.news,sample.twitter)
corpus<- Corpus(VectorSource(list(sample.corpus)))

# There are many sources for finding the list of words that have been classified as profane. The source for this analysis is the link that follows.http://www.freewebheaders.com/full-list-of-bad-words-banned-by-google/

profanity_list <- read.delim("profanity_list.txt",sep = ":",header = FALSE)
profanity<-profanity_list[,1]
rm(profanity_list)

# Remove profanity
corpus<- tm_map(corpus, content_transformer(removeWords), profanity)     


# Convert to lowercase
#corpus<-tm_map(corpus, content_transformer(stringi::stri_trans_tolower))
corpus<-tm_map(corpus, content_transformer(tolower))        

# Cleanse the data further by removing punctuation, numbers, and whitespace.
removeURL <- function(x) {
        gsub("http.*?( |$)", "", x)
}
corpus<-tm_map(corpus, content_transformer(removePunctuation)) # Remove punctuation
corpus<-tm_map(corpus, content_transformer(removeNumbers))     # Remove numbers
corpus<-tm_map(corpus, content_transformer(removeURL))         # Remove URL
#corpus<- tm_map(corpus, content_transformer(removeWords), stopwords("english"))  # Remove words like and,or,the etc.
corpus<-tm_map(corpus, content_transformer(stripWhitespace))  # Remove extra whitespace
writeCorpus(corpus, filenames ="my.corpus.txt")

###N-gram analysis

#Having created and cleaned the corpus, we can now perform n-gram analysis on the data. An n-gram is a contiguous sequence of n items from a given sequence of text or speech.  We will analyze unigrams, bigrams, trigrams, and quadrigrams using the RWeka package.


unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
quadrigramTokenizer<-function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
pentagramTokenizer<- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))

unigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = unigramTokenizer))
unigramMatrix<- removeSparseTerms(unigramMatrix, 0.99)
bigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = bigramTokenizer))
bigramMatrix<- removeSparseTerms(bigramMatrix, 0.999)
trigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = trigramTokenizer))
trigramMatrix<- removeSparseTerms(trigramMatrix, 0.999)
quadrigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = quadrigramTokenizer))
quadrigramMatrix<- removeSparseTerms(quadrigramMatrix, 0.999)
pentagramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize =pentagramTokenizer))
pentagramMatrix<- removeSparseTerms(pentagramMatrix, 0.999)

# termfreq <- function(tdm){
#         freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
#         termfreq <- data.frame(word=names(freq), freq=freq)
#         return(termfreq)
# }
# 
# 
# unigram.freq <- termfreq(unigramMatrix)
# bigram.freq <- termfreq(bigramMatrix)
# trigram.freq <- termfreq(trigramMatrix)
# quadrigram.freq <- termfreq(quadrigramMatrix)

pentagram.freq <- sort(rowSums(as.matrix(pentagramMatrix)), decreasing=TRUE)
pentagram.freq <- pentagram.freq[pentagram.freq >1]
pentagram.freq <- data.frame(uni=word(names(pentagram.freq),1),bi=word(names(pentagram.freq),2),tri=word(names(pentagram.freq),3), quad=word(names(pentagram.freq),4),penta=word(names(pentagram.freq),5),frequnecy=pentagram.freq)
saveRDS( pentagram.freq,file = "pentagram.Rdata")

quadrigram.freq <- sort(rowSums(as.matrix(quadrigramMatrix)), decreasing=TRUE)
quadrigram.freq <- quadrigram.freq[quadrigram.freq >1]
quadrigram.freq <- data.frame(uni=word(names(quadrigram.freq),1),bi=word(names(quadrigram.freq),2),tri=word(names(quadrigram.freq),3), quad=word(names(quadrigram.freq),4),frequnecy=quadrigram.freq)

saveRDS( quadrigram.freq,file = "quadrigram.Rdata")

trigram.freq <- sort(rowSums(as.matrix(trigramMatrix)), decreasing=TRUE)
trigram.freq <- trigram.freq[trigram.freq >1]
trigram.freq<-data.frame(uni=word(names(trigram.freq),1),bi=word(names(trigram.freq),2),tri=word(names(trigram.freq),3),frequnecy=trigram.freq)
saveRDS( trigram.freq,file = "trigram.Rdata")

bigram.freq <- sort(rowSums(as.matrix(bigramMatrix)), decreasing=TRUE)
bigram.freq <- bigram.freq[bigram.freq >1]
bigram.freq<-data.frame(uni=word(names(bigram.freq),1),bi=word(names(bigram.freq),2),frequnecy=bigram.freq)
saveRDS( bigram.freq,file = "bigram.Rdata")

unigram.freq <- sort(rowSums(as.matrix(unigramMatrix)), decreasing=TRUE)
unigram.freq <- unigram.freq[unigram.freq >1]
unigram.freq<-data.frame(uni=word(names(unigram.freq)),frequnecy=unigram.freq)
saveRDS( unigram.freq,file = "unigram.Rdata")

