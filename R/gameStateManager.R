playTurn <- function(currentGameState) {
  possibleTurnRows <- which(currentGameState$Deck == 'turn')
  if(length(possibleTurnRows) > 0) {
    cardToPlayRow <- sample(possibleTurnRows, 1)
    currentGameState[cardToPlayRow]$Visible <- TRUE
    currentGameState[cardToPlayRow]$Deck = 'discard'
    
    currentTurnRow <- which(currentGameState$Name == 'CurrentTurn')
    currentTurn <- currentGameState[currentTurnRow]$CurrentPosition
    currentTurnIncremented <- as.integer(currentTurn) + 1
    currentGameState[currentTurnRow]$CurrentPosition = as.character(currentTurnIncremented)
  }
  
  currentGameState
}