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
      if (self$opponent_greeting == "Lemon!") {
        self$bid <- "defect"
      }
      else {
        if (self$round == 1) {
          self$bid <- "cooperate"
        }
        else if (self$round > 1) {
          opp_id_one <- which(book$id1 == self$opponent_id)
          opp_id_two <- which(book$id2 == self$opponent_id)
          
          if (length(opp_id_one) >= 1 && length(opp_id_two) >= 1) {
            if (max(opp_id_one) > max(opp_id_two)) {
              self$bid <- as.vector(book$bid1[max(opp_id_one)])
            }
            else {
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