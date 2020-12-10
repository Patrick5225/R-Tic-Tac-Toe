# Tic-Tac-Toe program
# Author: Patrick Chao
# Date: 1/27/19
# Description: This program resembles standard gameplay of a round of tic-tac-toe. The program will ask if there are one or two human players, and then print out a gameboard where the player can play tic-tac-toe either against another human player or a computer player.

# ways to win
triples <- list(
  c(1,2,3),
  c(4,5,6),
  c(7,8,9),
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(1,5,9),
  c(3,5,7)
)

state <- as.character(1:9) # used to update the board
endGame <- FALSE # if TRUE, end game

display <- function(state) { # displays the board
	cat(" ", state[1], "|", state[2], "|", state[3], "\n")
	cat(" ---+---+---", "\n")
	cat(" ", state[4], "|", state[5], "|", state[6], "\n")
	cat(" ---+---+---", "\n")
	cat(" ", state[7], "|", state[8], "|", state[9], "\n")
}

prompt_user <- function(who,state) { # asks user for input, checks invalid or not
	while (endGame == FALSE) {	
		if (who == "x") { # 2 players
			choice <- as.character(readline("Choose where x should play: "))
			if (all(choice != 1:9)) { # if user inputs invalid number {
				cat("Invalid input. Please enter a number from 1 to 9","\n")	
				} else if (state[as.numeric(choice)] == "x" | state[as.numeric(choice)] == "o")
					{ # check to see if position already taken
					cat("Position already taken! Please enter a valid move", "\n")
				} else { # valid move
					state <- update(state,"x",as.numeric(choice))
					display(state)
					who <- "o"
					endGame <- check_winner(state)
					next
				}	
			} else if (who == "o") {
				choice <- as.character(readline("Choose where o should play: "))
				if (all(choice != 1:9)) { # if user inputs invalid number {
					cat("Invalid input. Please enter a number from 1 to 9","\n")	
				} else if (state[as.numeric(choice)] == "x" | state[as.numeric(choice)] == "o")
					{ # check to see if position already taken
					cat("Position already taken! Please enter a valid move", "\n")
				} else { # valid move
					state <- update(state,"o",as.numeric(choice))
					display(state)
					who <- "x"
					endGame <- check_winner(state)
					next
				}
			} else if (who == "xc") { # 1 player, computer plays first
				state <- computer_turn(state)
				display(state)
				endGame <- check_winner(state)
				who <- "o" # used temporarily to check for valid input
				while(who == "o" & endGame == FALSE) {
				choice <- as.character(readline("Choose where o should play: "))
				if (all(choice != 1:9)) { # if user inputs invalid number {
					cat("Invalid input. Please enter a number from 1 to 9","\n")	
				} else if (state[as.numeric(choice)] == "x" | state[as.numeric(choice)] == "o")
					{ # check to see if position already taken
					cat("Position already taken! Please enter a valid move", "\n")
				} else { # valid move
					state <- update(state,"o",as.numeric(choice))
					display(state)
					who <- "xc"
					endGame <- check_winner(state)
					next
					}
				}
			} else if (who == "xh") { # 1 player, human plays first
				who <- "x" # used temporarily to check for valid input
				while(who == "x" & endGame == FALSE) {
				choice <- as.character(readline("Choose where x should play: "))
				if (all(choice != 1:9)) { # if user inputs invalid number {
					cat("Invalid input. Please enter a number from 1 to 9","\n")	
				} else if (state[as.numeric(choice)] == "x" | state[as.numeric(choice)] == "o")
					{ # check to see if position already taken
					cat("Position already taken! Please enter a valid move", "\n")
				} else { # valid move
					state <- update(state,"x",as.numeric(choice))
					display(state)
					who <- "xh"
					endGame <- check_winner(state)
					if (endGame == TRUE) {
						return()
					}
					next
					}
				}				
				state <- computer_turn(state)
				display(state)
				endGame <- check_winner(state)
		}
	}
}

update <- function(state, who, choice) { # updates the board, placing x or o as needed
	state[choice] <- who
	cat(who,"plays at position",choice,"\n")
	return(state)
}

check_winner <- function(state) { # checks if there is a winner, then continue or end game
	for(i in 1:length(triples)) {
		if(all(triples[[i]] %in% which(state == "x"))) { # "x" has 3 in a row
			cat("x wins!")
			endGame <- TRUE
			return(endGame)
		}
	}
	for(i in 1:length(triples)) {
		if(all(triples[[i]] %in% which(state == "o"))) { # "o" has 3 in a row
			cat("o wins!")
			endGame <- TRUE
			return(endGame)
		}
	}
	if (all(state[1:9] == "x" | state[1:9] == "o")) { # every position is "x" or "o"
		cat("All positions taken. It's a draw.")
		endGame <- TRUE
		return(endGame)
	}
	return(FALSE) # no win or draw, so keep continuing the game
}

computer_turn <- function(state){
	if (sum(state == "x") > sum(state == "o")) {# computer goes second
    board <- state
    for(i in 1:length(triples)) {
      if (sum(triples[[i]] %in% which(state == "o")) == 2 && 
any(state[triples[[i]][which((triples[[i]] %in% which(state == "o")) == FALSE)]] %in% as.character(1:9)) && any(state[triples[[i]][which((triples[[i]] %in% which(state == "x")) == FALSE)]] %in% as.character(1:9))) {# computer attempts to win
		comp.position <- state[triples[[i]][which((triples[[i]] %in% which(state == "o")) == FALSE)]]
        state[as.numeric(comp.position)] <- "o"
		cat("o plays at position",comp.position,"\n")
        }
      if(identical(state,board) == FALSE)
		break
      }
    if(identical(state,board) == TRUE) {
      for(i in 1:length(triples)) {
        if(sum(triples[[i]] %in% which(state=="x")) == 2 && 
           any(state[triples[[i]][which((triples[[i]] %in% which(state=="x")) == FALSE)]] %in% as.character(1:9))&& any(state[triples[[i]][which((triples[[i]] %in% which(state=="o")) == FALSE)]] %in% as.character(1:9))){ # computer attempts to block
			comp.position <- state[triples[[i]][which((triples[[i]] %in% which(state == "x")) == FALSE)]]
          state[as.numeric(comp.position)] <- "o"
		cat("o plays at position",comp.position,"\n")
          }
        if(identical(state,board) == FALSE)
		break
      }
    }
    if(identical(state,board) == TRUE) { # play random position
	comp.position <- state[sample(which(state == as.character(1:9)),1)]
	state[as.numeric(comp.position)] <- "o"
	cat("o plays at position",comp.position,"\n")
		}
    } else if (sum(state=="o") >= sum(state=="x") || sum(state=="o") == 0){ #computer goes first 
      board <- state
      for(i in 1:length(triples)) {
        if(sum(triples[[i]] %in% which(state == "x")) == 2 &&
           any(state[triples[[i]][which((triples[[i]] %in% which(state == "x")) == FALSE)]] %in% as.character(1:9)) && any(state[triples[[i]][which((triples[[i]] %in% which(state == "o")) == FALSE)]] %in% as.character(1:9))){ # computer attempts to win
			comp.position <- state[triples[[i]][which((triples[[i]] %in% which(state == "x")) == FALSE)]]
          state[as.numeric(comp.position)]<-"x"
			cat("x plays at position",comp.position,"\n")
          }
        if(identical(state,board) == FALSE)
		break
        }
      if(identical(state,board) == TRUE){
        for(i in 1:length(triples)){
          if(sum(triples[[i]] %in% which(state == "o")) == 2 && 
             any(state[triples[[i]][which((triples[[i]] %in% which(state == "o")) == FALSE)]] %in% as.character(1:9)) && any(state[triples[[i]][which((triples[[i]] %in% which(state == "x")) == FALSE)]] %in% as.character(1:9))){ # computer attempts to block
			comp.position <- state[triples[[i]][which((triples[[i]] %in% which(state == "o")) == FALSE)]]
            state[as.numeric(comp.position)] <- "x"
			cat("x plays at position",comp.position,"\n")
            }
          if(identical(state,board) == FALSE)
		break
        }
      }
      if(identical(state,board) == TRUE) { # play random position
		reset <- TRUE
		while(reset == TRUE) { # make sure computer plays at a valid spot
		comp.position <- sample(which(state == as.character(1:9)),1)
		if (state[as.numeric(comp.position)] == "x" | state[as.numeric(comp.position)] == "o") {
			reset <- TRUE
		} else
		reset <- FALSE
		}
		state[as.numeric(comp.position)] <- "x"
		cat("x plays at position",comp.position,"\n")
		}
      }
  return(state)
 } 

play <- function() { # program starts here
	firstOrSecond <- 0 # used to determine who plays first
	playersAmnt <- readline("Enter the amount of human players (1 or 2): ")
	if (playersAmnt == 1) { # 1 human player
		while((firstOrSecond != 1) & (firstOrSecond != 2)) {
		firstOrSecond <- readline("Should the computer play first or second (1 or 2): ")
			if (firstOrSecond == 1) { # computer plays first
				display(state)
				prompt_user("xc",state) # xc = computer plays "x"
			} else if (firstOrSecond == 2) { # human plays first
				display(state)
				prompt_user("xh",state) # xh = human plays "x"
			} else { # invalid input of first or second
				cat("Invalid input. Please enter 1 or 2.")
			} 
		}
	} else if (playersAmnt == 2) { # 2 humans players
		display(state)
		prompt_user("x",state) 
	} else { # invalid input of number of players
		cat("Invalid input. Please enter 1 or 2.")
		return(play())
	}
} 