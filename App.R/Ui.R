library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  #going with a black theme
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel("Word Prediction Model"),
  
  #sidebar layout which has a main and side panel
  sidebarLayout(
    
    sidebarPanel(
      textInput("prompt","Put your input here!"),
      actionButton("predict","Predict"),
      h5("The predicted word is:"),
      h4(textOutput("text"))),
    
    mainPanel(
      h4("Next Word predictor using Natural Language Processing"),
      h5("User Information:"),
      h5("Type your input string of words in the text box & click the predict button, it will generate 
         a predicted word output along with a boxplot of words with similar frequency"),
      h5("The model might take some time to generate and plot the output, so please be patient"))),
      plotOutput("plot",height = 700)

)
