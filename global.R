
library(qdapDictionaries)
library(data.table)
library(rvest)
library(dplyr)

non_letters <- c()

letters <- c('', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')

# Create an empty list for each of the lock letters
lock_letters <- list(lock_letter_1 = NULL, 
                     lock_letter_2 = NULL, 
                     lock_letter_3 = NULL, 
                     lock_letter_4 = NULL, 
                     lock_letter_5 = NULL)

# Create an empty list for each of the anti-lock letters
anti_lock_letters <- list(anti_lock_letter_1 = NULL, 
                          anti_lock_letter_2 = NULL, 
                          anti_lock_letter_3 = NULL, 
                          anti_lock_letter_4 = NULL, 
                          anti_lock_letter_5 = NULL)

# Get a list of all 5-letter words
initial_word_list <- function(){
  # Access the GradyAugmented dataset
  word_list <- GradyAugmented
  
  # Filter for 5-letter words
  five_letter_words <- word_list[nchar(word_list) == 5]
  
  #break up five_letter_words into a data.table where each letter is sorted into a single  column, ie. WORDS becomes a dataframe with five columns: letter_one: W, letter_two: O, and so on
  # Convert the list into a data.table
  words_dt <- data.table(word = five_letter_words)
  
  
  # Split each word into letters and store them in separate columns
  words_dt[, c("letter_1", "letter_2", "letter_3", "letter_4", "letter_5") := 
             tstrsplit(word, split = "", fixed = TRUE)]
  
  # Remove rows with any non-alphabet character in any column
  words_dt <- words_dt[!apply(words_dt, 1, function(row) any(grepl("[^A-Za-z]", row)))]
  
  return(words_dt)
}

# List of words already used by Wordl
used_list <- function(){
  url <- "https://www.rockpapershotgun.com/wordle-past-answers"
  
  # Read the HTML content of the page
  page <- read_html(url)
  
  # Extract the word list
  word_list <- page %>%
    html_nodes(xpath = "/html/body/div[1]/div/div[2]/div[1]/div[1]/main/div/article/div/div/ul[2]") %>%
    html_text() %>%
    strsplit(" ") %>%
    unlist()
  
  # Split word_list at each \n 
  word_list <- strsplit(word_list, "\n") %>% unlist()
  
  return(word_list)
}

# Remove words based on the known letters
letter_analyzer <- function(current_word_list, lock_letters, non_letters, anti_lock_letters) {
  
  #if lock_letter_1 is null, then select all rows, same for lock_letter 2-5.  However, if lock_letter_1 has a value, only select those rows where letter_1 is equal to lock_letter_1 and the same for lock_letters 2-5
  if(lock_letters$lock_letter_1 != ''){
    current_word_list <- start[start$letter_1 == lock_letters$lock_letter_1,]
  } else {
    current_word_list <- start
  }
  if(lock_letters$lock_letter_2 != ''){
    current_word_list <- current_word_list[current_word_list$letter_2 == lock_letters$lock_letter_2,]
  }
  if(lock_letters$lock_letter_3 != ''){
    current_word_list <- current_word_list[current_word_list$letter_3 == lock_letters$lock_letter_3,]
  }
  if(lock_letters$lock_letter_4 != ''){
    current_word_list <- current_word_list[current_word_list$letter_4 == lock_letters$lock_letter_4,]
  }
  if(lock_letters$lock_letter_5 != ''){
    current_word_list <- current_word_list[current_word_list$letter_5 == lock_letters$lock_letter_5,]
  }
  
  #### NON LETTERS ####
  if(length(non_letters) != 0){
    current_word_list <- current_word_list[!current_word_list$letter_1 %in% non_letters,]
    current_word_list <- current_word_list[!current_word_list$letter_2 %in% non_letters,]
    current_word_list <- current_word_list[!current_word_list$letter_3 %in% non_letters,]
    current_word_list <- current_word_list[!current_word_list$letter_4 %in% non_letters,]
    current_word_list <- current_word_list[!current_word_list$letter_5 %in% non_letters,]
  }
  
  #### ANTI LOCK LETTERS ####
  # if(anti_lock_letters$anti_lock_letter_1 != ''){
  if(length(anti_lock_letters$anti_lock_letter_1) != 0){

    print(current_word_list)
    for (i in 1:length(anti_lock_letters$anti_lock_letter_1)) {
      current_word_list <- current_word_list[current_word_list$letter_1 != anti_lock_letters$anti_lock_letter_1[i] &
                                               (current_word_list$letter_2 == anti_lock_letters$anti_lock_letter_1[i] |
                                                  current_word_list$letter_3 == anti_lock_letters$anti_lock_letter_1[i] |
                                                  current_word_list$letter_4 == anti_lock_letters$anti_lock_letter_1[i] |
                                                  current_word_list$letter_5 == anti_lock_letters$anti_lock_letter_1[i]),]
    }
  }
  
  # if(anti_lock_letters$anti_lock_letter_2  != ''){
  if(length(anti_lock_letters$anti_lock_letter_2) != 0){
    
    for (i in 1:length(anti_lock_letters$anti_lock_letter_2)) {
      current_word_list <- current_word_list[current_word_list$letter_2 != anti_lock_letters$anti_lock_letter_2[i] &
                                               (current_word_list$letter_1 == anti_lock_letters$anti_lock_letter_2[i] |
                                                  current_word_list$letter_3 == anti_lock_letters$anti_lock_letter_2[i] |
                                                  current_word_list$letter_4 == anti_lock_letters$anti_lock_letter_2[i] |
                                                  current_word_list$letter_5 == anti_lock_letters$anti_lock_letter_2[i]),]
    }
  }
  
  # if(anti_lock_letters$anti_lock_letter_3 != ''){
  if(length(anti_lock_letters$anti_lock_letter_3) != 0){
    
    for (i in 1:length(anti_lock_letters$anti_lock_letter_3)) {
      current_word_list <- current_word_list[current_word_list$letter_3 != anti_lock_letters$anti_lock_letter_3[i] &
                                               (current_word_list$letter_2 == anti_lock_letters$anti_lock_letter_3[i] |
                                                  current_word_list$letter_1 == anti_lock_letters$anti_lock_letter_3[i] |
                                                  current_word_list$letter_4 == anti_lock_letters$anti_lock_letter_3[i] |
                                                  current_word_list$letter_5 == anti_lock_letters$anti_lock_letter_3[i]),]
    }
  }
  
  # if(anti_lock_letters$anti_lock_letter_4 != ''){
  if(length(anti_lock_letters$anti_lock_letter_4) != 0){
    
    for (i in 1:length(anti_lock_letters$anti_lock_letter_4)) {
      current_word_list <- current_word_list[current_word_list$letter_4 != anti_lock_letters$anti_lock_letter_4[i] &
                                               (current_word_list$letter_2 == anti_lock_letters$anti_lock_letter_4[i] |
                                                  current_word_list$letter_3 == anti_lock_letters$anti_lock_letter_4[i] |
                                                  current_word_list$letter_1 == anti_lock_letters$anti_lock_letter_4[i] |
                                                  current_word_list$letter_5 == anti_lock_letters$anti_lock_letter_4[i]),]
    }
  }
  
  # if(anti_lock_letters$anti_lock_letter_5 != ''){
  if(length(anti_lock_letters$anti_lock_letter_5) != 0){
    for (i in 1:length(anti_lock_letters$anti_lock_letter_5)) {
      current_word_list <- 
        current_word_list[current_word_list$letter_5 != anti_lock_letters$anti_lock_letter_5[i] &
                            (current_word_list$letter_2 == anti_lock_letters$anti_lock_letter_5[i] |
                               current_word_list$letter_3 == anti_lock_letters$anti_lock_letter_5[i] |
                               current_word_list$letter_4 == anti_lock_letters$anti_lock_letter_5[i] |
                               current_word_list$letter_1 == anti_lock_letters$anti_lock_letter_5[i]),]
    }
  }
  
  return(current_word_list)
  
}

# Word Scoring Function
word_score <- function(possible_words){
  words_dt <- possible_words
  
  # Remove all columns but letter_1 through letter_5 and word
  words_dt <- words_dt[, c("word", "letter_1", "letter_2", "letter_3", "letter_4", "letter_5")]
  
  # Melt the data.table to a long format with one column for letters
  long_dt <- melt(words_dt, measure.vars = patterns("^letter_[0-9]$"), value.name = "letter")
  
  # Count occurrences of each letter
  letter_counts <- long_dt[, .N, by = letter][order(-N)]
  
  # Rename columns
  setnames(letter_counts, c("letter", "count"))
  
  # Loop through each letter column to assign counts
  for (i in 1:5) {
    letter_col <- paste0("letter_", i)
    count_col <- paste0("letter_", i, "_count")
    
    # Drop the existing letter_i_count column (count_col) if it exists
    if (count_col %in% names(words_dt)) {
      words_dt[, (count_col) := NULL]
    }
    
    # Merge letter counts to get the score for the column
    words_dt <- merge(words_dt, letter_counts, by.x = letter_col, by.y = "letter", all.x = TRUE)
    
    # Rename the merged count column
    setnames(words_dt, "count", count_col)
  }
  
  
  # Calculate the total score for each word
  score_cols <- grep("_count$", names(words_dt), value = TRUE)  # Select all score columns
  words_dt[, total_count := rowSums(.SD, na.rm = TRUE), .SDcols = score_cols]
  
  # Adjust scores for repeated letters
  for (i in 1:nrow(words_dt)) {
    # Get the letters and scores for the current row
    row_letters <- unlist(words_dt[i, .(letter_1, letter_2, letter_3, letter_4, letter_5)])
    row_scores <- unlist(words_dt[i, .(letter_1_count, letter_2_count, letter_3_count, letter_4_count, letter_5_count)])
    
    # Track first occurrences of letters
    seen_letters <- character(0)
    adjusted_scores <- sapply(seq_along(row_letters), function(j) {
      if (row_letters[j] %in% seen_letters) {
        1  # Score of 1 for repeated letters
      } else {
        seen_letters <<- c(seen_letters, row_letters[j])
        row_scores[j]  # Original score for the first occurrence
      }
    })
    
    # Update the row with adjusted scores
    for (j in 1:5) {
      score_col <- paste0("letter_", j, "_score")
      words_dt[i, (score_col) := adjusted_scores[j]]
    }
  }
  
  
  # Calculate the total score for each word
  score_cols <- grep("_score$", names(words_dt), value = TRUE)  # Select all score columns
  words_dt[, total_mod_score := rowSums(.SD, na.rm = TRUE), .SDcols = score_cols]
  
  return(words_dt)
}
initial_words <- initial_word_list()
# word_list <- initial_word_list()