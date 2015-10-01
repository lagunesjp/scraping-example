# http://beyondvalence.blogspot.mx/2014/01/text-mining-3-stemming-text-and.html
library(rvest)
library(stringi)
library(plyr)
library(dplyr)
library(SnowballC)
library(tm)


load(file = "datos/mdpi.Rdata")

# create corpus
abstr_corpus <- mdpi$abstract %>%
  as.character() %>%
  VectorSource() %>%
  Corpus()

length(abstr_corpus)
abstr_corpus[[1]]$content
# transform corpus
corpus_transf <- abstr_corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords('en'))
corpus_transf[[1]]$content

# stemming
corpus_dict <- corpus_transf
corpus_stem <- corpus_transf %>%
  tm_map(stemDocument)
corpus_stem[[1]]$content

# problems with stemming
# corpus_stem <- tm_map(corpus_stem, stemCompletion_modified, dictionary = corpus_dict, 
#   mc.cores = 1)
# corpus_stem[[1]]
# 
# stemCompletion_mod <- function(x) {
#   PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=corpus_dict, type="shortest"),sep="", collapse=" ")))
# }
# corpus_stem2 <- lapply(corpus_stem, stemCompletion_mod)
# corpus_stem2[[1]]$content

# corpus_stem3 <- tm_map(corpus_stem, content_transformer(function(x, corpus_dict)
#   paste(stemCompletion(strsplit(stemDocument(x), ' ')[[1]], corpus_dict), collapse = ' ')), corpus_dict)

# Document-Term matrix
tdm <- TermDocumentMatrix(corpus_stem, control = list(minWordLength = 1))
tdm
freq_terms <- findFreqTerms(tdm, lowfreq = 10)
freq_terms

# word cloud
library(wordcloud)
tdm_mat <- as.matrix(tdm)
# calculate frequency of words and sort
word_freq <- sort(rowSums(tdm_mat), decreasing = TRUE)
wordcloud(words = names(word_freq), freq = word_freq, min.freq = 5, 
  random.order = FALSE)
