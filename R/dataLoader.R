loadInitialGameData <- function() {
  library(data.table)
  
  ally <<- fread("data/ally.csv")
  artifact <<- fread("data/artifact.csv")
  direEnemy <<- fread("data/direEnemy.csv")
  enemy <<- fread("data/enemy.csv")
  epicFoe <<- fread("data/epicFoe.csv")
  hero <<- fread("data/hero.csv")
  region <<- fread("data/region.csv")
}

getRandomRegions <- function() {
  randomRegions <- c(1, sample(2:3, 1), sample(4:5, 1), sample(6:7, 1), sample(8:9, 1), sample(10:11, 1), sample(12:13, 1))
}

setupGameState <- function(numberOfPlayers, difficulty) {
  currentGameState <- list()
  currentGameState$ally <- ally[1:4,]
  currentGameState$direEnemy <- direEnemy[sample(6,1),]
  tempSampleForEnemyList <- sample(9,6)
  currentGameState$enemyStart <- enemy[tempSampleForEnemyList[1:3],]
  currentGameState$enemyHorde <- enemy[tempSampleForEnemyList[4:6],]
  currentGameState$epicFoe <- epicFoe[sample(6,1),]
  currentGameState$hero <- hero[sample(10,2),]
  currentGameState$region <- region[getRandomRegions(),]
  currentGameState
}
