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

setupGameState <- function(numberOfPlayers, difficulty) {
  currentGameState <<- baseGameData
  allyRows <- which(currentGameState$Type == 'ally')
  currentGameState[allyRows[(numberOfPlayers-1):(numberOfPlayers+2)],]$Deck <<- 'turn'
  
  heroRows <- which(currentGameState$Type == 'hero')
  pickedHeroRows <- sample(heroRows, numberOfPlayers)
  currentGameState[pickedHeroRows,]$Deck <<- 'hero'
  currentGameState[pickedHeroRows,]$CurrentPosition <<- '7'
  
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
  
  randomOrderForLand <<- sample(1:6, 6)
  assignPositionToRegion('Desert', randomOrderForLand[1])
  assignPositionToRegion('Ruins', randomOrderForLand[2])
  assignPositionToRegion('Forest', randomOrderForLand[3])
  assignPositionToRegion('Mountains', randomOrderForLand[4])
  assignPositionToRegion('Plains', randomOrderForLand[5])
  assignPositionToRegion('Coast', randomOrderForLand[6])
  
  currentGameState[which(currentGameState$Name == 'CurrentGameGuid'),]$CurrentPosition <<- UUIDgenerate()
}
