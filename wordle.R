wordle <- function(){
  require(words) #make sure the library is there
  require(cli) #color fun
  possibleWords <- words$word[words$word_length == 5] #get 5 letter words
  set.seed(Sys.Date()) #set seed according to day
  sample(possibleWords,1) |> strsplit("") |> getElement(1) -> word #get word and split into letters 
  
  message("Welcome to the woRdle for ", format(Sys.Date(), "%d %B, %Y. \nType EXIT at any time to stop."))
  
  #now we begin getting guesses
  i <- 1
  wrongLetters <- c() #not asked for, but this is a nice feature...
  
  while(i < 7){
    readline("Guess? ") |> tolower() -> guess
    
    #exit option
    if(guess == "exit") return(message("Ending game. Come back soon."))
    
    #handle bad guesses
    while(! guess %in% possibleWords){
      message("That is not a valid 5 letter word.")
      readline("Guess? ") |> tolower() -> guess
    }
    
    guess <- strsplit(guess,"")[[1]]
    correct <- guess == word #which letters are correct
    
    #end game on correct guess, with a message
    if(all(correct)) return(
      message("Well done! The word was ", 
              paste0(word, collapse=""),
              ". You got it in ", i, " guesses.")
      )
    
    toCheck <- guess[!correct] #letters we need to check for placement
    remaining <- word[!correct] #letters left in true word
    
    inWord <- vector(length=length(toCheck))
    for(j in seq_along(toCheck)){
      index <- grep(toCheck[[j]],remaining)[1]
      if(is.na(index)) next
      remaining <- remaining[-index] #remove from remaining
      inWord[j] <- T
    }
    
    guess[correct] <- toupper(guess[correct])
    guess[((1:5)[!correct])[!inWord]] <- "*"
    
    c(wrongLetters, toCheck[!inWord]) |> unique() |> sort() -> wrongLetters
    
    message("Result: ",paste0(guess,collapse=""))
    message("You have ", 6-i, " remaining guesses.")
    message("Letters guessed that are not in word: ", paste0(wrongLetters,collapse=""))
    cat(bg_black(col_green("Letters you haven't tried: ", paste0(letters[!letters %in% wrongLetters],collapse=""))))
    i <- i + 1
  
  }
  
  message("\nSorry! The word was ", word,". Try again tomorrow.")
  
}



