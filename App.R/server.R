# loading the libraries

require(tm)
require(ngram)
require(ggplot2)
require(stringi)
require(stringr)
require(dplyr)


library(shiny)


# loading the data
unigram <- readRDS("unigram.rds")
bigram <- readRDS("bigram.rds")
trigram <- readRDS("trigram.rds")
quadgram <- readRDS("quadgram.rds")



Text_clean <- function(input)  {
  
  #convert all words to lowercase
  Text <- str_to_lower(input)
  
  # getting rid of slash and hypons
  Text <- gsub("-", ' ', Text)
  Text <- gsub("/", ' ', Text)
  
  
  #removes any remaining <> brackets
  Text <- gsub("<>", ' ', Text)
  
  #removes apostrophe but doesnt change the word
  Text <- gsub("'", '', Text)
  
  #remove punctuation
  Text <- gsub('[[:punct:] ]+', ' ', Text)
  
  #remove numbers
  Text <- gsub("[0-9]+", "", Text)
  
  #remove extra whitespace
  Text <- gsub("\\s+", " ", Text)
  Text <- str_trim(Text)
  
  #return the cleaned user input
  return(Text)
  
}


# function that takes the cleaned input then assigns 
# the input into grams depending on the words and a pred_df



df_ngram <- function(text) {
  
  userInput <- unlist(strsplit(text," "))      #to split the text into individual strings
  userInput_length <- length(userInput)    #then checking the length
  
  
  if(userInput_length >= 3) {                      #if only the input text has 3 or more than 3 words
    
    grams <- paste(userInput[userInput_length-2],  #putting the trigrams in a variable
                   userInput[userInput_length-1],
                   userInput[userInput_length])
    
    pred_df <- quadgram                            #will be using the quadragram to determine & predict the next word after 3 word input
    
  }  else if (userInput_length == 2) {                    #if only the input text is 2 words
    
    grams <- paste(userInput[userInput_length-1],  #putting the Bigrams in a variable
                   userInput[userInput_length])
    
    pred_df <- trigram                             #will be using the trigams df to determine & predict the next word after 2 word input 
    
  } else {                                         #if the input text is 1 word
    
    grams <- paste(userInput[userInput_length])    #putting the unigram in a variable
    
    pred_df <- bigram                              #will be using the bigram df to determine the next word after the word
  }
  assign("grams",grams,envir = .GlobalEnv)
  assign("pred_df",pred_df,envir = .GlobalEnv) 
  
}

# backoff model that looks foe matches b/w the input and the pred_df
# if no then it will reduce n-1(the first input word)
# also reduces the pred_df by one


backoff <- function(input_ngram) {
  
  matched_ngram <- sum(pred_df$ngrams == grams) #checking if there are any matches in the pred_df with the input
  
  #if there are no word matches in that specific ngram then we try to reduce the ngrams by so that we eventually get a match
  
  if(matched_ngram == 0) {               #if there are no matches(ie matched_ngram == 0)
    
    if(length(pred_df$ngrams) == length(bigram$ngrams))  { #if the target df is bigram we cant make any more change in the input grams
      
      grams <- input_ngram          
      pred_df <- unigram  #but we change its pred_df to unigram so it can atleast go with word that is used alot in uni df
      
      matched_ngram <- 0 
      
    } else { 
      
      if(length(pred_df$ngrams) == length(trigram$ngrams))  {  #if the target df is trigram(i.e 2 worded input) we make changes by reducing the ngram
        
        grams <- word(input_ngram,-1)                        #this only gets the last word of the gram,so changing it from a 2 to 1 word input
        pred_df <- bigram                             #changing the prediction df to bigram from trigram
        
        matched_ngram <- sum(pred_df$ngrams == grams)
        
      } else {                                          #the last condition is that the input is 3 worded(the pred_df will be quadgram)
        
        grams <- word(input_ngram,-2,-1)              #this only gets the last 2 words of the input instead of 3 worded input
        pred_df <- trigram                              #changing the prediction df to trigram from quadgram
        
        
        matched_ngram <- sum(pred_df$ngrams == grams)   #now checking again to see if are there any matches or not
     

        
      }
      
      
    }  
    
  }
  
  assign("matched_ngram",matched_ngram,envir = .GlobalEnv)
  assign("pred_df",pred_df,envir = .GlobalEnv)
  assign("grams",grams,envir = .GlobalEnv)
  
}  





predicted_word <- function(pred_df,grams) {
  
  if (length(pred_df$ngrams) == length(unigram$ngrams)) {     #for the input that didnt match with bigram so had to be changed to unigram
    
    prediction <- unigram %>%     #then going with the words that have most freq 
      top_n(n=3,wt = prop) %>%    #in the unigram df
      slice(1:3)
    
    prediction_df <- as.data.frame(prediction)
    
  }  else {                    #if the input did have a macth
    
    prediction <- pred_df %>%     #gets the macthed rows with the highest freq in that specific df
      filter(grepl(paste0("^", grams, "\\b"),ngrams)) %>%
      top_n(n = 3,wt = prop) %>%
      slice(1:3)
    
    prediction_df <- as.data.frame(prediction)
  }
  
  assign("prediction_df",prediction_df,envir = .GlobalEnv)
  
}  










# Define server logic

function(input, output, session) {
  
  user_input <- eventReactive(input$predict, {     #making the input text reactive i.e changes with the change in Ui

    
    input$prompt
                                
    
  })
  
  
  # for the output text that is to be printed in the sidebar.
  output$text <- renderText ({
    
    # checking if the predict button is pressed or not 
    if(input$predict == 0) {
      # if no return
      return()
      
    }  else  {
      #if yes ,then we validate whether something is written as a prompt or not
     validate(
       #if no then will give this message
       need(input$prompt != '','Please type something')
     
       )

    # Now going through the prediction pipeline  
    # cleaning the input text
    clean_input <- Text_clean(input$prompt)        
    
    # assigning grams & the dataset used for prediction          
    df_ngram(clean_input)
    
    # using the backoff model if no match in the assigned dataset & gram
    backoff(grams)
    
    
    # now getting the words with most freq for the inout
    predicted_word(pred_df,grams) 

      
    #printing the word with the most freq
    return(prediction_df$ngrams[1])
     
    } 
    
    
  })
  
  
  
  # now the plot that is going to be printed 
  
  output$plot <- renderPlot({

    #validating if there is any input
    validate(
      #if no will give this message
      need(input$prompt != '',"no input")

          )
    # checking if the predict button is pressed or not
    if (input$predict == 0) {
      
    return() 
      
    }  else  {
    
    # Now going through the prediction pipeline  
    # cleaning the input text
    clean_input <- Text_clean(input$prompt)        
    
    # assigning grams & the dataset used for prediction          
    df_ngram(clean_input)
    
    # using the backoff model if no match in the assigned dataset & gram
    backoff(grams)
    
    
    # now getting the words with most freq for the inout
    final_result <- predicted_word(pred_df,grams)
    
      # plotting the 3 words with most freq
      g <- ggplot(prediction_df, aes(x = reorder(ngrams, -freq), y = freq)) + 
        geom_bar(stat = "identity",fill = "blue") + theme(axis.text.x = element_text(hjust = 1.0, angle = 45),
                                                            axis.text.y = element_text(hjust = 0.5, vjust = 0.5))
      
      print(g)
      
    
    }
      
      
  })
  
  
}
