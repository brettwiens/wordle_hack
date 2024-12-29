#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  observeEvent(input$submit, {

    if (input$skip_used == TRUE){
      current_word_list <- initial_words[!(initial_words$word %in% tolower(used_list())),]
    } else {
      current_word_list <- initial_words
    }

    lock_letters <- list(
      lock_letter_1 = tolower(input$lock_letter_1),
      lock_letter_2 = tolower(input$lock_letter_2),
      lock_letter_3 = tolower(input$lock_letter_3),
      lock_letter_4 = tolower(input$lock_letter_4),
      lock_letter_5 = tolower(input$lock_letter_5)
    )

    anti_lock_letters <- list(
      anti_lock_letter_1 = unlist(strsplit(tolower(input$anti_lock_letter_1), "")),
      anti_lock_letter_2 = unlist(strsplit(tolower(input$anti_lock_letter_2), "")),
      anti_lock_letter_3 = unlist(strsplit(tolower(input$anti_lock_letter_3), "")),
      anti_lock_letter_4 = unlist(strsplit(tolower(input$anti_lock_letter_4), "")),
      anti_lock_letter_5 = unlist(strsplit(tolower(input$anti_lock_letter_5), ""))
    )
    
    # Function to clean each list
    anti_lock_letters <- lapply(anti_lock_letters, function(vec) {
      # Keep only alphabetic elements
      vec[grepl("^[a-zA-Z]$", vec)]
    })

    no_letter <- c(input$non_letters)
    #break no_letter into individual letters
    no_letter <- unlist(strsplit(no_letter, ""))
    
    current_word_list <- letter_analyzer(current_word_list, lock_letters, no_letter, anti_lock_letters)
    
    current_word_list <- word_score(current_word_list)

    output$word_table <- DT::renderDT(current_word_list[order(current_word_list$total_mod_score, decreasing = TRUE), c("word", "total_mod_score")], options = list(pageLength = 10))


  })

}
