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

getRegionRow <- function(enemyOrDireRow, shouldDoSecondRegion = FALSE) {
  ifelse(shouldDoSecondRegion, 
         which(currentGameState$Name == currentGameState[enemyOrDireRow,]$Region2 & currentGameState$Deck == 'region'),
         which(currentGameState$Name == currentGameState[enemyOrDireRow,]$Region & currentGameState$Deck == 'region'))
}

handleHeroAttemptToDefend <- function(enemyToPlayRow, shouldDoSecondRegion = FALSE) {
  regionRow1 <- getRegionRow(enemyToPlayRow)
  regionCurrentPosition <- currentGameState[regionRow1,]$CurrentPosition
  heroesThatCanDefendRows <- which(currentGameState$Type == 'hero' & currentGameState$CurrentPosition == regionCurrentPosition & currentGameState$CurrentHealth > 0)
  choiceToMake <- sample(length(heroesThatCanDefendRows) + 1, 1)
  if(choiceToMake > length(heroesThatCanDefendRows)) {
    dealRegionDamage(regionRow1)
  } else {
    heroToTakeDamageRow <- heroesThatCanDefendRows[choiceToMake]
    currentGameState[heroToTakeDamageRow,]$CurrentHealth <<- currentGameState[heroToTakeDamageRow,]$CurrentHealth - 1
  }
}

handleEnemyCard <- function(enemyToPlayRow) {
  handleHeroAttemptToDefend(enemyToPlayRow)
  handleHeroAttemptToDefend(enemyToPlayRow, TRUE)
}

handleDireEnemyCard <- function(direEnemyToPlayRow) {
  direEnemyAbility <- currentGameState[direEnemyToPlayRow]$Ability
  if(direEnemyAbility == 'TSUNAMI') {
    heroesInOuterRegions <- which(currentGameState$Type == 'hero' & currentGameState$Deck != 'destruction' & currentGameState$CurrentPosition != '7')
    
  }
  dealRegionDamage(getRegionRow(direEnemyToPlayRow))
  if(direEnemyAbility == 'FRENZY') {
    currentGameState[which(currentGameState$Deck == 'region' & currentGameState$CurrentHealth == '4' & currentGameState$MaxHealth != 7)]$CurrentHealth <<- 3
  }
  if(direEnemyAbility == 'RAZE') {
    dealRegionDamage(getRegionRow(direEnemyToPlayRow))
  }
  if(direEnemyAbility == 'STING') {
    heroesWithFullHealth <- which(currentGameState$Type == 'hero' & currentGameState$CurrentHealth == currentGameState$MaxHealth & currentGameState$Deck != 'destruction')
    currentGameState[heroesWithFullHealth,]$CurrentHealth <<- currentGameState[heroesWithFullHealth,]$CurrentHealth - 1
  }
  if(direEnemyAbility == 'DRAIN') {
    heroes <- which(currentGameState$Type == 'hero' & currentGameState$Deck != 'destruction')
    heroToBeDrained <- sample(heroes, 1)
    currentGameState[heroToBeDrained,]$CurrentHealth <<- max(c(0, currentGameState[heroToBeDrained,]$CurrentHealth - 2))
  }
}

selectTheHero <- function(nameOfAlly, heroRows) {
  if(nameOfAlly == 'group') {
    sample(heroRows, 1)
  } else {
    which(currentGameState$Deck == nameOfAlly)
  }
}

handleAllyCard <- function(allyToPlayRow) {
  name <- currentGameState[allyToPlayRow,]$Name
  heroRows <- which(currentGameState$Type == 'hero' & currentGameState$Deck != 'destruction')
  selectedHero <- selectTheHero(name, heroRows)
  
  selectedHero <- which(currentGameState$Deck == name)
  currentPositionAsInt <- as.integer(currentGameState[selectedHero,]$CurrentPosition)
  if(currentPositionAsInt != 7) {
    howToMoveAroundBoard <- sample(c(-1,1), 1)
    currentPositionAsInt <- (currentPositionAsInt + howToMoveAroundBoard) %% 6
    currentGameState[selectedHero,]$CurrentPosition <<- as.character(currentPositionAsInt)
  } else {
    currentGameState[selectedHero,]$CurrentPosition <<- '3'
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
  } else if (currentGameState[cardToPlayRow,]$Type == 'ally') {
    handleAllyCard(cardToPlayRow)
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
