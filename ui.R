library(shiny)

fluidPage(
  # Add custom CSS for the green selectizeInput class
  tags$style(HTML("
    .green-selectize .selectize-control {
      background-color: green !important;
    }
  ")),
  
  
    # Application title
    titlePanel("Wordle Hack"),
    br(),
  hr(),
  br(),
    column(2, 
           shiny::checkboxInput(inputId = "skip_used", 
                                label = "Skip used words", 
                                value = TRUE)),
    column(6,
           
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
                       value = "")),
           fluidRow(
             actionButton(inputId = "submit", 
                          label = "Submit")
           )
           ),
    column(4,
           img(src = "logo.png", height = 150),
           DT::DTOutput("word_table"))

)
