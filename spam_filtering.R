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
    con <- file(path, encoding = "latin1")
    text <- readLines(con)
    # The message always begins after the first full line break
    msg <- text[seq(which(text == "")[1] + 1, length(text), 1)]
    close(con)
    return(paste(msg, collapse = "\n"))
}

# Get all the SPAM-y email into a single vector
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs != "cmds")]
all.spam <- sapply(spam.docs,
                   function(p) get.msg(file.path(spam.path, p)))

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

# Create a data frame that provides the feature set from the training SPAM data
spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)
spam.df <- data.frame(cbind(names(spam.counts),
                            as.numeric(spam.counts)),
                      stringsAsFactors = FALSE)
names(spam.df) <- c("term", "frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)
spam.occurrence <- sapply(1:nrow(spam.matrix),
                          function(i)
                          {
                              length(which(spam.matrix[i, ] > 0)) / ncol(spam.matrix)
                          })
spam.density <- spam.df$frequency / sum(spam.df$frequency)

# Add the term density and occurrence rate
spam.df <- transform(spam.df,
                     density = spam.density,
                     occurrence = spam.occurrence)
head(spam.df[with(spam.df,order(-occurrence)),])

# Now do the same for the EASY HAM email
ham.docs <- dir(easyham.path)
ham.docs <- ham.docs[1:500]
all.ham <- sapply(ham.docs,function(p) get.msg(file.path(easyham.path,p)))
ham.tdm <- get.tdm(all.ham)
ham.matrix <- as.matrix(ham.tdm)
ham.counts <- rowSums(ham.matrix)
ham.df <- data.frame(cbind(names(ham.counts),as.numeric(ham.counts)),
                           stringsAsFactors=F)
names(ham.df)<-c('term','frequency')
ham.df$frequency <- as.numeric(ham.df$frequency)
ham.occurrence <- sapply(1:nrow(ham.matrix), 
                         function(i) {
                             length(which(ham.matrix[i,]>0))/ncol(ham.matrix)
                         })
ham.density <- ham.df$frequency/sum(ham.df$frequency)
ham.df <- transform(ham.df, density=ham.density,occurrence=ham.occurrence)

# classifier
classify.email <- function(path,training.df,prior=.5, c=1e-6){
    msg <- get.msg(path)
    msg.tdm <- get.tdm(msg)
    msg.freq <- rowSums(as.matrix(msg.tdm))
    # find intersections of words
    msg.match <- intersect(names(msg.freq),training.df$term)
    if(length(msg.match)<1){
        return(prior*c^(length(msg.freq)))
    }
    else{
        match.probs <- training.df$occurrence[match(msg.match,training.df$term)]
        return(prior*prod(match.probs)*c^(length(msg.freq)-length(msg.match)))
    }
}

# test
hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs!='cmds')]

hardham.spamtest <- sapply(hardham.docs,
                           function(p)
                               classify.email(paste(hardham.path,p,sep='/'),training.df=spam.df))

hardham.hamtest <- sapply(hardham.docs,
                          function(p) classify.email(paste(hardham.path,p,sep='/'),
                                                     training.df=ham.df))

hardham.res <- ifelse(hardham.spamtest>hardham.hamtest,TRUE,FALSE)
summary(hardham.res)

# test all email types
spam.classifier <- function(path){
    pr.spam <- classify.email(path,spam.df,prior=.2)
    pr.ham <- classify.email(path,ham.df,prior=.8)
    return(c(pr.spam,pr.ham,ifelse(pr.spam>pr.ham,1,0)))
}

easyham2.docs <- dir(easyham2.path)
easyham2.docs <- easyham2.docs[which(easyham2.docs != "cmds")]

hardham2.docs <- dir(hardham2.path)
hardham2.docs <- hardham2.docs[which(hardham2.docs != "cmds")]

spam2.docs <- dir(spam2.path)
spam2.docs <- spam2.docs[which(spam2.docs != "cmds")]

easyham2.class <- suppressWarnings(lapply(easyham2.docs,
                                          function(p)
                                          {
                                              spam.classifier(file.path(easyham2.path, p))
                                          }))
hardham2.class <- suppressWarnings(lapply(hardham2.docs,
                                          function(p)
                                          {
                                              spam.classifier(file.path(hardham2.path, p))
                                          }))
spam2.class <- suppressWarnings(lapply(spam2.docs,
                                       function(p)
                                       {
                                           spam.classifier(file.path(spam2.path, p))
                                       }))

easyham2.matrix <- do.call(rbind, easyham2.class)
easyham2.final <- cbind(easyham2.matrix, "EASYHAM")

hardham2.matrix <- do.call(rbind, hardham2.class)
hardham2.final <- cbind(hardham2.matrix, "HARDHAM")

spam2.matrix <- do.call(rbind, spam2.class)
spam2.final <- cbind(spam2.matrix, "SPAM")

class.matrix <- rbind(easyham2.final, hardham2.final, spam2.final)
class.df <- data.frame(class.matrix, stringsAsFactors = FALSE)
names(class.df) <- c("Pr.SPAM" ,"Pr.HAM", "Class", "Type")
class.df$Pr.SPAM <- as.numeric(class.df$Pr.SPAM)
class.df$Pr.HAM <- as.numeric(class.df$Pr.HAM)
class.df$Class <- as.logical(as.numeric(class.df$Class))
class.df$Type <- as.factor(class.df$Type)
summary(class.df)
head(class.df)

# Create final plot of results
class.plot <- ggplot(class.df, aes(x = log(Pr.HAM), log(Pr.SPAM))) +
    geom_point(aes(shape = Type, alpha = 0.5)) +
    stat_abline(yintercept = 0, slope = 1) +
    scale_shape_manual(values = c("EASYHAM" = 1,
                                  "HARDHAM" = 2,
                                  "SPAM" = 3),
                       name = "Email Type") +
    scale_alpha(guide = "none") +
    xlab("log[Pr(HAM)]") +
    ylab("log[Pr(SPAM)]") +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank())
ggsave(plot = class.plot,
       filename = file.path("images", "final_classification.pdf"),
       height = 10,
       width = 10)

get.results <- function(bool.vector)
{
    results <- c(length(bool.vector[which(bool.vector == FALSE)]) / length(bool.vector),
                 length(bool.vector[which(bool.vector == TRUE)]) / length(bool.vector))
    return(results)
}


# Save results as a 2x3 table
easyham2.col <- get.results(subset(class.df, Type == "EASYHAM")$Class)
hardham2.col <- get.results(subset(class.df, Type == "HARDHAM")$Class)
spam2.col <- get.results(subset(class.df, Type == "SPAM")$Class)

class.res <- rbind(easyham2.col, hardham2.col, spam2.col)
colnames(class.res) <- c("NOT SPAM", "SPAM")
print(class.res)

# Save the training data for use in Chapter 4
write.csv(spam.df, file.path("data", "spam_df.csv"), row.names = FALSE)
write.csv(ham.df, file.path("data", "easyham_df.csv"), row.names = FALSE)
