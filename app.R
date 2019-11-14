#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("model2.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Word Prediction"),
    
    # Sidebar with text input box
    sidebarLayout(
        sidebarPanel(
            textInput("inputtext", "Enter text", "enter your input here"),
            checkboxInput("removeStopwords", label = "Remove Stopwords"),
            submitButton('Predict!')
        ),
        mainPanel(
            
            tabsetPanel(type = "tab",
                        tabPanel("Prediction", 
                                 h3("Prediction"),
                                 dataTableOutput("result")),
                        
                        tabPanel("Documentation", 
                                 h3("Instructions"), 
                                 p("In order to use this application, enter text in the box provided and click 'Predict!' to see the predictions."),
                                 h3("Method"),
                                 p("The algorithm uses a stupid backoff model. First the model starts with a 5-gram match, given the sentence is long enough. If there is a match, the probability of the word is calculated based on the 5-gram match. If there is not a match, it moves onto 4-gram, to 3-gram, and so on."),
                                 p("This model gives absolute priority to a higher n-gram match so it will not check other lower n-grams if there is a match already. For example, if there is a 5-gram match, regardless of the accuracy of the prediction, the model will stop and not check for 4-gram or lower n-gram matches. In most cases, this does not cause issues but is something to be wary of."),
                                 h3("Stopwords"),
                                 p("Stopwords are words that are very common in a language such as 'I', 'a', 'you'. Removing these words can possibly improve or worsen the prediction. The accuracy depends on the complexity of the sentence. For example, if you input a sentence such as 'I'll be on my', then the prediction with stopwords included would provide a better result since the input contains many stopwords. In contrast, if you are predicting based on a sentence such as 'Flowers and plants are both very' then it may be better to exclude the stopwords so that the algorithm predicts on the words 'flowers' and 'plants' rather than 'are both very'.")
                        )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
    output$result <- renderDataTable({
        if(input$removeStopwords){
            backoff_nostop(input$inputtext)
        }
        else{
            backoff_stop(input$inputtext)
        } 
    }, options = list(pageLength = 5, lengthMenu = c(5, 10)))
    
}


# Run the application 
shinyApp(ui = ui, server = server)

