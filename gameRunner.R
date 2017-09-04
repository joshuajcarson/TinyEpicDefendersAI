source('R/dataLoader.R')
source('R/gameStateManager.R')

loadInitialGameData()
setupGameState(4, 1)

while(!isEnemyWon() && !isPlayersWon()) {
  playTurn()
}
