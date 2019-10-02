library(R6)

Agent <- R6Class(
  "Agent",
  
  public = list(
    bid = NULL,
    book = NULL,
    greeting = NULL,
    id = NULL,
    opponent_id = NULL,
    opponent_greeting = NULL,
    round = NULL,
    response = NULL,
    
    set_book = function(book = NA) {
      self$book <- book
    },
    
    set_id = function(id = NA) {
      self$id = id
    },
    
    set_opponent_id = function(opponent_id = NA) {
      self$opponent_id = opponent_id
    },
    
    set_response = function(response = NA) {
      self$response <- response
    },
    
    set_round = function(round = NA) {
      self$round <- round
    },
    
    set_greeting = function() {
      self$greeting <- "Hi, my name is agent 13!"
    },
    
    receive_greeting = function(greeting = NA) {
      self$opponent_greeting = greeting
    },
    
    get_bid = function() {
      #There are agents in there that only defect, and have "Lemon!" as a greeting
      #This is the way to filter them out
      if (self$opponent_greeting == "Lemon!") {
        self$bid <- "defect"
      }
      
      else {
        #Tit-for-tat strategy is based on being cooperative the first round
        if (self$round == 1) {
          self$bid <- "cooperate"
        }
        
        #After that we take the last bid of our opponent 
        #(the last bid against anyone, right now)
        else if (self$round > 1) {
          #We're searching for all the rows in which our opponent played
          opp_id_one <- which(book$id1 == self$opponent_id)
          opp_id_two <- which(book$id2 == self$opponent_id)
          
          #For the max function to work, both columns need one occurence
          #Otherwise we cooperate
          if (length(opp_id_one) >= 1 && length(opp_id_two) >= 1) {
            #Check which row contains the last bid of our opponent
            if (max(opp_id_one) > max(opp_id_two)) {
              #Get the value in col 'bid1' on that last row, and copy it as our bid
              self$bid <- as.vector(book$bid1[max(opp_id_one)])
            }
            else {
              #Get the value in col 'bid2' on that last row, and copy it as our bid
              self$bid <- as.vector(book$bid2[max(opp_id_one)])
            }
          }
          else {
            self$bid <- "cooperate"
          }
        }
      }
    },
    
    formulate_bid = function() {
      self$get_bid()
    }
  )
)