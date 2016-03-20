#Sample a new tweet assuming a Markov chain model.

source("markov_chain.R")

PrepareTweets <- function(data) {
  # Transforms the supplied tweets into a character vector to be used as the argument of the SimulateTweets-function.
  #
  # Args:
  #   data: A vector containing the observed tweets.
  #
  # Returns:
  #   A vector of the tweets split to characters and cleaned from punctuation.

  data <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", data)
  data <- gsub("@\\w+", "", data)
  data <- gsub("(?!#)[[:punct:]]", "", data, perl=TRUE)
  data <- gsub("http\\w+", "", data)
  data <- gsub("https\\w+", "", data)
  data <- gsub("[ \t]{2,}", "", data)
  data <- gsub("^\\s+|\\s+$", "", data)
  data <- gsub("\n","",data)
  
  # define "tolower error handling" function
  try.tolower <- function(x)
  {
    y <- NA
    try_error <- tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y <- tolower(x)
    return(y)
  }
  
  data = sapply(data, try.tolower)
  names(data) = NULL

  data <- paste(data, collapse=" ")
  data <- strsplit(data, split="")[[1]]

  data
}

SimulateTweet <- function(data, tweet.length, chain.order) {
    # Samples a new tweet from the supplied tweets assuming a Markov Chain model.
    #
    # Args:
    #   data: A vector output by the function PrepareTweets.
    #   tweet.length: Maximum length of the output tweet.
    #   chain.order: Order of the assumed Markov Chain model.
    #
    # Returns:
    #   A new tweet of maximum length tweet.length.

    tweet <- SimulateMarkovChain(data, tweet.length, 50, chain.order)

    order <- sort(which(tweet == " "), TRUE)
    last <- order[1]
    order <- -cumsum(diff(order))
    first <- last - tail(order[order <= tweet.length], 1)[1]
    tweet <- tweet[(first + 1):(last - 1)]
    capitalize <- which(tweet == ".")
    tweet[capitalize + 2] <- toupper(tweet[capitalize + 2])
    tweet[1] <- toupper(tweet[1])
    tweet <- paste(tweet, collapse="")
    tweet <- gsub(" .", ".", tweet, fixed=TRUE)
    tweet
}
