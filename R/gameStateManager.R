incrementTurn <- function() {
  currentTurnRow <- which(currentGameState$Name == 'CurrentTurn')
  currentTurn <- currentGameState[currentTurnRow]$CurrentPosition
  currentTurnIncremented <- as.integer(currentTurn) + 1
  currentGameState[currentTurnRow]$CurrentPosition <<- as.character(currentTurnIncremented)
}

dealRegionDamage <- function(regionRow) {
  if(currentGameState[regionRow,]$CurrentHealth > 0) {
    currentGameState[regionRow,]$CurrentHealth <<- currentGameState[regionRow,]$CurrentHealth - 1
  } else {
    currentGameState[which(currentGameState$Name == 'Capital City'),]$CurrentHealth <<- currentGameState[which(currentGameState$Name == 'Capital City'),]$CurrentHealth - 1
  }
}

getRegionRow <- function(enemyOrDireRow, do2 = false) {
  ifelse(do2, 
         which(currentGameState$Name == currentGameState[enemyOrDireRow,]$Region2 & currentGameState$Deck == 'region'),
         which(currentGameState$Name == currentGameState[enemyOrDireRow,]$Region & currentGameState$Deck == 'region'))
}

handleEnemyCard <- function(enemyToPlayRow) {
  region1row <- which(currentGameState$Name == currentGameState[enemyToPlayRow,]$Region & currentGameState$Deck == 'region')
  region2row <- which(currentGameState$Name == currentGameState[enemyToPlayRow,]$Region2 & currentGameState$Deck == 'region')
  dealRegionDamage(region1row)
  dealRegionDamage(region2row)
}

handleDireEnemyCard <- function(direEnemyToPlayRow) {
  direEnemyAbility <- currentGameState[direEnemyToPlayRow]$Ability
  if(direEnemyAbility == 'FRENZY') {
    currentGameState[which(currentGameState$Deck == 'region' && currentGameState$CurrentHealth == '4' && currentGameState$MaxHealth != 7)]$CurrentHealth <<- 3
  }
  regionRow <- which(currentGameState$Name == currentGameState[direEnemyToPlayRow,]$Region & currentGameState$Deck == 'region')
  dealRegionDamage(regionRow)
  if(direEnemyAbility == 'RAZE') {
    dealRegionDamage(regionRow)
  }
}

playACard <- function(possibleTurnRows) {
  cardToPlayRow <- ifelse(length(possibleTurnRows) == 1, possibleTurnRows, sample(possibleTurnRows, 1))
  currentGameState[cardToPlayRow,]$Visible <<- TRUE
  currentGameState[cardToPlayRow,]$Deck <<- 'discard'
  if(currentGameState[cardToPlayRow,]$Type == 'enemy') {
    handleEnemyCard(cardToPlayRow)
  } else if (currentGameState[cardToPlayRow,]$Type == 'direEnemy') {
    handleDireEnemyCard(cardToPlayRow)
  }
  
  incrementTurn()
}

finishRoundAndStartNext <- function() {
  discardRow <- which(currentGameState$Deck == 'discard')
  currentGameState[discardRow]$Deck <<- 'turn'
  
  possibleHordeRows <- which(currentGameState$Deck == 'horde')
  if(length(possibleHordeRows) > 0) {
    hordeRow <- ifelse(length(possibleHordeRows) == 1, possibleHordeRows, sample(possibleHordeRows, 1))
    currentGameState[hordeRow,]$Deck <<- 'turn'
  } else {
    currentGameState[which(currentGameState$Deck == 'epicFoe')]$Visible <<- TRUE
  }
  
  currentRoundRow <- which(currentGameState$Name == 'CurrentRound')
  currentRound <- currentGameState[currentRoundRow]$CurrentPosition
  currentRoundIncremented <- as.integer(currentRound) + 1
  currentGameState[currentRoundRow]$CurrentPosition <<- as.character(currentRoundIncremented)
}

isPlayersWon <- function() {
  epicFoeRow <- which(currentGameState$Deck == 'epicFoe')
  currentGameState[epicFoeRow]$CurrentHealth == 0
}

isEnemyWon <- function() {
  capitalCity <- which(currentGameState$Name == "Capital City")
  currentGameState[capitalCity]$CurrentHealth < 1
}

playTurn <- function() {
  possibleTurnRows <- which(currentGameState$Deck == 'turn')
  if(length(possibleTurnRows) > 0) {
    playACard(possibleTurnRows)
  } else {
    finishRoundAndStartNext()
  }
}
