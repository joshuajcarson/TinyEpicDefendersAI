source('R/dataLoader.R')
source('R/gameStateManager.R')

baseGameData <- loadInitialGameData()
currentGameState <- setupGameState(baseGameData, 3, 1)

currentGameState <- playTurn(currentGameState)
