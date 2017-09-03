loadInitialGameData <- function() {
  library(data.table)
  library(uuid)
  
  fread("data/allCards.csv")
}

setupGameState <- function(baseGameData, numberOfPlayers, difficulty) {
  allyRows <- which(baseGameData$Type == 'ally')
  baseGameData[allyRows[(numberOfPlayers-1):(numberOfPlayers+2)],]$Deck = 'turn'
  
  heroRows <- which(baseGameData$Type == 'hero')
  pickedHeroRows <- sample(heroRows, numberOfPlayers)
  baseGameData[pickedHeroRows,]$Deck = 'hero'
  baseGameData[pickedHeroRows,]$CurrentPosition = '7'
  
  enemyRows <- which(baseGameData$Type == 'enemy')
  pickedEnemyRows <- sample(enemyRows, 6)
  baseGameData[pickedEnemyRows[1:3],]$Deck = 'turn'
  baseGameData[pickedEnemyRows[4:6],]$Deck = 'horde'
  
  direEnemyRows <- which(baseGameData$Type == 'direEnemy')
  pickedDireEnemyRows <- sample(direEnemyRows, difficulty)
  baseGameData[pickedDireEnemyRows,]$Deck = 'horde'

  epicFoeRows <- which(baseGameData$Type == 'epicFoe')
  pickedEpicFoeRows <- sample(epicFoeRows, 1)
  baseGameData[pickedEpicFoeRows,]$Deck = 'epicFoe'
  
  randomOrderForLand <- sample(1:6, 6)
  baseGameData[sample(which(baseGameData$Name == 'Desert'), 1),]$CurrentPosition = as.character(randomOrderForLand[1])
  baseGameData[sample(which(baseGameData$Name == 'Ruins'), 1),]$CurrentPosition = as.character(randomOrderForLand[2])
  baseGameData[sample(which(baseGameData$Name == 'Forest'), 1),]$CurrentPosition = as.character(randomOrderForLand[3])
  baseGameData[sample(which(baseGameData$Name == 'Mountains'), 1),]$CurrentPosition = as.character(randomOrderForLand[4])
  baseGameData[sample(which(baseGameData$Name == 'Plains'), 1),]$CurrentPosition = as.character(randomOrderForLand[5])
  baseGameData[sample(which(baseGameData$Name == 'Coast'), 1),]$CurrentPosition = as.character(randomOrderForLand[6])
  baseGameData[which(baseGameData$Type == 'region' & baseGameData$CurrentPosition != 'destruction'),]$Deck = 'region'
  
  baseGameData[which(baseGameData$Name == 'CurrentGameGuid'),]$CurrentPosition = UUIDgenerate()
  
  baseGameData
}
