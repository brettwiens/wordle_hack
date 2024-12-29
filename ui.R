library(shiny)

fluidPage(
  tags$head(
    tags$style(HTML("
      .green-selectize .selectize-input {
        background-color: limegreen !important;
        color: white !important;
        font-weight: bold;
      }
    "))
  ),
  
  
    # Application title
    titlePanel("Wordle Hack"),
    
    column(2, 
           shiny::checkboxInput(inputId = "skip_used", 
                                label = "Skip used words", 
                                value = TRUE)),
    column(6,
           fluidRow(
             actionButton(inputId = "submit", 
                          label = "Submit")
           ),
           fluidRow(
             column(2,
                    shiny::selectizeInput(inputId = "lock_letter_1", 
                                     label = "Lock Letter 1", 
                                     choices = letters,
                                     options = list(class = "green-selectize"))),
             column(2,
                    shiny::selectizeInput(inputId = "lock_letter_2", 
                                     label = "Lock Letter 2", 
                                     choices = letters,
                                     options = list(class = "green-selectize"))),
             column(2,
                    shiny::selectizeInput(inputId = "lock_letter_3", 
                                     label = "Lock Letter 3", 
                                     choices = letters,
                                     options = list(class = "green-selectize"))),
             column(2,
                    shiny::selectizeInput(inputId = "lock_letter_4", 
                                     label = "Lock Letter 4", 
                                     choices = letters,
                                     options = list(class = "green-selectize"))),
             column(2,
                    shiny::selectizeInput(inputId = "lock_letter_5", 
                                     label = "Lock Letter 5", 
                                     choices = letters,
                                     options = list(class = "green-selectize")))
             ),
           fluidRow(
             column(2,
                    shiny::textInput(inputId = "anti_lock_letter_1",
                                     label = "Anti-Lock Letter 1",
                                     value = "")),
             column(2,
                    shiny::textInput(inputId = "anti_lock_letter_2",
                                     label = "Anti-Lock Letter 2",
                                     value = "")),
             column(2,
                    shiny::textInput(inputId = "anti_lock_letter_3",
                                     label = "Anti-Lock Letter 3",
                                     value = "")),
             column(2,
                    shiny::textInput(inputId = "anti_lock_letter_4",
                                     label = "Anti-Lock Letter 4",
                                     value = "")),
             column(2,
                    shiny::textInput(inputId = "anti_lock_letter_5",
                                     label = "Anti-Lock Letter 5",
                                     value = ""))),
           fluidRow(
             textInput(inputId = "non_letters",
                       label = "Non-Letters *comma separated*",
                       value = ""))
           ),
    column(4,
           img(src = "logo.png", height = 150),
           DT::DTOutput("word_table"))

)
