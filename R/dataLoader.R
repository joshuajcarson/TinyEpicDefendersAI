loadInitialGameData <- function() {
  library(data.table)
  library(uuid)
  
  baseGameData <<- fread("data/allCards.csv")
}

randomRegionRow <- function(regionName) {
  sample(which(currentGameState$Name == regionName), 1)
}

assignPositionToRegion <- function(regionName, currentPosition) {
  pickedRandomRow <- randomRegionRow(regionName)
  currentGameState[pickedRandomRow,]$CurrentPosition <<- as.character(currentPosition)
  currentGameState[pickedRandomRow,]$Deck <<- 'region'
}

getAlliesThatAreNotGroup <- function() {
  which(currentGameState$Deck != 'destruction' & currentGameState$Type == 'ally' & currentGameState$Name != 'group')
}

assignHeroToColor <- function() {
  heroRows <- which(currentGameState$Type == 'hero')
  colorRows <- getAlliesThatAreNotGroup()
  heroRows <- sample(heroRows, length(colorRows))
  currentGameState[heroRows]$CurrentPosition <<- '7'
  currentGameState[heroRows]$Deck <<- currentGameState[colorRows]$Name
}

setupGameState <- function(numberOfPlayers, difficulty) {
  currentGameState <<- baseGameData
  allyRows <- which(currentGameState$Type == 'ally')
  currentGameState[allyRows[(numberOfPlayers-1):(numberOfPlayers+2)],]$Deck <<- 'turn'
  
  assignHeroToColor()
  
  enemyRows <- which(currentGameState$Type == 'enemy')
  pickedEnemyRows <- sample(enemyRows, 6)
  currentGameState[pickedEnemyRows[1:3],]$Deck <<- 'turn'
  currentGameState[pickedEnemyRows[4:6],]$Deck <<- 'horde'
  
  direEnemyRows <- which(currentGameState$Type == 'direEnemy')
  pickedDireEnemyRows <- sample(direEnemyRows, difficulty)
  currentGameState[pickedDireEnemyRows,]$Deck <<- 'horde'

  epicFoeRows <- which(currentGameState$Type == 'epicFoe')
  pickedEpicFoeRows <- sample(epicFoeRows, 1)
  currentGameState[pickedEpicFoeRows,]$Deck <<- 'epicFoe'
  
  randomOrderForLand <- sample(0:5, 6)
  assignPositionToRegion('Desert', randomOrderForLand[1])
  assignPositionToRegion('Ruins', randomOrderForLand[2])
  assignPositionToRegion('Forest', randomOrderForLand[3])
  assignPositionToRegion('Mountains', randomOrderForLand[4])
  assignPositionToRegion('Plains', randomOrderForLand[5])
  assignPositionToRegion('Coast', randomOrderForLand[6])
  
  currentGameState[which(currentGameState$Name == 'CurrentGameGuid'),]$CurrentPosition <<- UUIDgenerate()
  currentGameState[which(currentGameState$Name == 'DifficultySelected'),]$CurrentPosition <<- as.character(difficulty)
}
