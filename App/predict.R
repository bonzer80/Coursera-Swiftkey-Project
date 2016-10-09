suppressPackageStartupMessages(c(
        library(shinythemes),
        library(shiny),
        library(tm),
        library(stringr),
        library(markdown),
        library(stylo)))

# Import n-grams to be used for prediction

pentaData <- as.data.frame(readRDS("./myData/pentagram.Rdata"))
quadData <- as.data.frame(readRDS("./myData/quadrigram.Rdata"))
triData <-as.data.frame(readRDS("./myData/trigram.Rdata"))
biData <-as.data.frame(readRDS("./myData/bigram.Rdata"))

# Clean the data input by the user


cleaner <- function(text){
    
        cleanInput <- tolower(text)
        cleanInput <- removePunctuation(cleanInput)
        cleanInput <- removeNumbers(cleanInput)
        cleanInput <- str_replace_all(cleanInput, "[^[:alnum:]]", " ")
        cleanInput <- stripWhitespace(cleanInput)
        cleanInput <- txt.to.words.ext(cleanInput, 
                                      language="English.all", 
                                      preserve.case = TRUE)
        
        return(cleanInput)
        wordCount <- length(cleanInput)
}

# Predict the next word

# If the input string has more than 4 or more words, take the last 4 words of the string.If not, pad the 
# string with the appropriate number of NAs to have get  a 4 word string. 

predictor <- function(wordCount,cleanInput){
        if (wordCount>=4) {
                cleanInput <- cleanInput[(wordCount-3):wordCount] 
                
        }
        else if (wordCount==3) {
                cleanInput <- c(NA,cleanInput)  
                
        }
        
        else if(wordCount==2) {
                cleanInput <- c(NA,NA,cleanInput)   
        }
        
        else {
                cleanInput <- c(NA,NA,NA,cleanInput)
        }
        
# We will start with pentagrams and move down to lower level grams to predict the next word. Assign the first word
# of the input string as the first word of the pentagram, the second word of the input string as the second of the
# pentagram and so on. If the input string is present in the pentagram data, the fifth word of the pentagram will
# be the predicted word. If it is not present, go down to a lower level n-gram and repeat the logic.
               
prediction <- as.character(pentaData[pentaData$uni==cleanInput[1] &  
                                                         pentaData$bi==cleanInput[2] &  
                                                         pentaData$tri==cleanInput[3] & 
                                                         pentaData$quad==cleanInput[4],][1,]$penta)           
        if(is.na(prediction)) { 
                as.character(quadData[quadData$uni==cleanInput[2] &  
                                              quadData$bi==cleanInput[3] &  
                                              quadData$tri==cleanInput[4],][1,]$quad)
                if(is.na(prediction)) { 
                        prediction <- as.character(triData[triData$uni==cleanInput[3] &  
                                                                       triData$bi==cleanInput[4],][1,]$tri) 
                } 
                if(is.na(prediction)) { 
                        prediction <- as.character(biData[biData$uni==cleanInput[4],][1,]$bi) 
                } 
                
        }
        print(prediction) 
}
        
        
  