setwd('C:\\Documents and Settings\\Macro\\Desktop\\Ivandata\\spam-filtering')
library('tm')
library('ggplot2')
spam.path <- file.path("data", "spam")
spam2.path <- file.path("data", "spam_2")
easyham.path <- file.path("data", "easy_ham")
easyham2.path <- file.path("data", "easy_ham_2")
hardham.path <- file.path("data", "hard_ham")
hardham2.path <- file.path("data", "hard_ham_2")

# Return a single element vector of just the email body
# This is a very simple approach, as we are only using 
# words as features
get.msg <- function(path)
{
    con <- file(path, open = "rt", encoding = "latin1")
    text <- readLines(con)
    # The message always begins after the first full line break
    msg <- text[seq(which(text == "")[1] + 1, length(text), 1)]
    close(con)
    return(paste(msg, collapse = "\n"))
}

# Get all the SPAM-y email into a single vector
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs != "cmds")]
all.spam <- sapply(spam.docs,function(p) get.msg(file.path(spam.path, p, sep='')))

# Create a TermDocumentMatrix (TDM) from the corpus of SPAM email.
# The TDM control can be modified, and the sparsity level can be 
# altered.  This TDM is used to create the feature set used to do 
# train our classifier.
get.tdm <- function(doc.vec) {
    doc.corpus <- Corpus(VectorSource(doc.vec))
    control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE,
                    minDocFreq=2)
    doc.dtm <- TermDocumentMatrix(doc.corpus, control)
    return(doc.dtm)
}
spam.tdm <- get.tdm(all.spam)