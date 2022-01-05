# import libraries
library(shiny)

source("wordle_script.R")

##################
# User Interface #
##################


ui <- fluidPage(
  titlePanel(
    h2("Wordle Guesser", align = "center")
    ),
  fluidRow(    
    align = "center",
    img(src = "wordle.png", height = 60, width = 90),
    br(),    
    "Want to excel at Wordle but don't know how?",
    br(),
    "Impress your friends but put in as little effort as possible?",
    br(),
    "Well, I am not sure if this is the right way to do it but here it is anyway:",
    br(),
    em("A Shiny App that helps you guess Wordle solutions based on"), em(HTML("<a href='https://coolbutuseless.github.io/2021/12/28/solving-a-wordle-with-assistance-from-r/' target='_blank'>this code</a>")), em("by "), em(HTML("<a href='https://twitter.com/coolbutuseless' target='_blank'>@coolbutuseless</a>")), em("."),
    br(),
    br(),
    strong("Just play Wordle as you normally would but after each guess you fill out the inputs below."),
    br(),
    strong("Once you filled all the inputs, click on 'Get Suggested Words' and try one of them."),
    br(),
    strong("Good luck and have fun!")
  ),
  hr(),
  titlePanel(
    h4("Letters in Correct Spot", align = "center")
    ),
  mainPanel(
    fluidRow(
      align = "center",
      splitLayout(
                cellWidths = c("5%", "5%", "5%", "5%", "5%"),
                textInput("l1", "1st"),
                textInput("l2", "2nd"),
                textInput("l3", "3rd"),
                textInput("l4", "4th"),
                textInput("l5", "5th")
            ),
      em("Type in the letters that were exact matches (in green).")
    ),
    titlePanel(
      h4("Letters in Wrong Spots", align = "center")
    ),    
    fluidRow(
      align = "center",
      splitLayout(
        cellWidths = c("5%", "5%", "5%", "5%", "5%"),
        textInput("w1", "1st"),
        textInput("w2", "2nd"),
        textInput("w3", "3rd"),
        textInput("w4", "4th"),
        textInput("w5", "5th")
      ),
      em("Type in letters that do occur but were in the wrong spot (in yellow)."),
      br(),
      em("Note: if you uncover multiple wrong letters in the same spot, type them in together: 'xyz'.")
    ),
    titlePanel(
      h4("Exclude letters", align = "center")
    ), 
    fluidRow(
      align = "center",
      textInput("excl", NULL),
      em("If letters don't occur at all (in grey) type them here like this: 'xyz'.")
    ),
    hr(),
    fluidRow(
      align = "center",
      actionButton("get_words", "Get Suggested Words",
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      br(),
      "Try one of the suggested words in your Wordle",
      shiny::verbatimTextOutput("choices")
    ),
    br(),
    em("Author of Shiny app:"), em(HTML("<a href='https://twitter.com/favstats' target='_blank'>Fabio Votta (@favstats)</a>")),
    br(),
    width = 12
  )
)


##########
# SERVER #
##########

server <- function(input, output, session) {
 
  source("https://raw.githubusercontent.com/coolbutuseless/wordle/main/R/words.R")
  
  observeEvent(input$get_words, {
    
    testEmpty <- function(x) x == "" | x == "."
    replaceEmpty <- function(x) ifelse(x == "", ".", x)
    
    
    lets <- c(input$l1, input$l2, input$l3, input$l4, input$l5)
    wrongs <-  c(input$w1, input$w2, input$w3, input$w4, input$w5)

    all_empty <- all(sapply(c(lets, wrongs), testEmpty))
        
    if(all_empty){
      
      choices <- sample(wordle_dict, size = ifelse(length(wordle_dict)<40, length(wordle_dict), 40))
      
    } else if (!all_empty){
      
      print(paste0(replaceEmpty(lets), collapse = ""))
      print(wrongs)
      
      choices <- filter_words(wordle_dict, 
                            exact = paste0(replaceEmpty(lets), collapse = ""), 
                            excluded_letters = input$excl,
                            wrong_spot = wrongs)

      choices <- sample(choices, size = ifelse(length(choices)<40, length(choices), 40))
      
      choices
    }
    
    
    output$choices <- renderPrint({ choices })
    
  })
  
  
  # renderPrint()
}

##############
# Launch App #
##############

#generic line that launches the app
shinyApp(ui = ui, server = server)