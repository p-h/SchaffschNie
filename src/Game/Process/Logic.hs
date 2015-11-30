 {-# LANGUAGE Arrows            #-}
module Game.Process.Logic where

import Data.Fixed

import FRP.Yampa

import Game.AppTypes
import Game.Types
import Game.Process.Event


-- Main Function
gameSF :: GameData -> SF AppInput GameData
gameSF gameData0 = dSwitch sf changeLevel
    where
        sf = proc appInput -> do
            nextSession <- gameSessionSF gameData0 -< appInput
            let nextGameData = setGameSession_ gameData0 nextSession
            -- take key 1, 2, 3 event for level selection
            changeLevelEvent <- levelTrigger -< appInput

            returnA -< (nextGameData, changeLevelEvent)

        changeLevel number = gameSF $ setGameLevel_ gameData0 number


-- Game Session
gameSessionSF :: GameData -> SF AppInput GameSession
gameSessionSF gameData0 = switch sf restartLevel
    where
        sf = proc appInput -> do
            gameSession <- moveWorldSF gameData0 -< ()

            nextGamePlayer <- playerFallingSF gameData0 -< (appInput, gameSession)
            let nextGameSession = gameSession { gPlayer = nextGamePlayer }
            let nextGameData = gameData0 { gSession = nextGameSession }

            -- check events
            endOfWorldEvent <- edge -< checkEndOfWorld nextGameData
            collisionEvent <- edge -< checkCollision nextGameData
            let restartLevelEvent = rMerge endOfWorldEvent collisionEvent

            returnA -< (nextGameSession, restartLevelEvent)

        restartLevel _ = gameSessionSF ajustedTriesGameData
            where
                ajustedTriesGameData = gameData0 { gSession = ajustedTriesGameSession }
                ajustedTriesGameSession = gameSession0 { gTries = (gTries gameSession0) + 1 }

        gameSession0 = gSession gameData0


-- Game Session Functions
checkEndOfWorld :: GameData -> Bool
checkEndOfWorld gameData = endReached
    where
        endReached = (getEndPositionOfLevel <= gPosX gameSession)
        getEndPositionOfLevel = (oPositionX $ last gameLevel) - 5
        gameSession = gSession gameData
        gameLevel = currentGameLevel gameData

checkCollision :: GameData -> Bool
checkCollision gameData = oColliding frontObj || oColliding playerObj || (oColliding topObj && isPlayerInTheAir gameData)
    where
        frontObj = getPlayerGameObject gameData 1 0
        playerObj = getPlayerGameObject gameData 0 0
        topObj = getPlayerGameObject gameData 0 1


-- World SF
moveWorldSF :: GameData -> SF a GameSession
moveWorldSF gameData0 = proc _ -> do
        nextPosX <- moveWorldPositionSF worldSpeed worldPosX -< ()
        returnA -< gameSession0 { gPosX = nextPosX }
        where
            gameSession0 = gSession gameData0
            worldPosX = (gPosX gameSession0)
            worldSpeed = gWorldSpeed $ gSettings gameData0


moveWorldPositionSF :: Double -> Double -> SF () Double
moveWorldPositionSF worldSpeed gPosX0 = constant worldSpeed >>> imIntegral gPosX0



-- Player SF State
playerDrivingSF :: GameData -> SF (AppInput, GameSession) GamePlayer
playerDrivingSF gameData0 = switch sf (\gameData -> playerFallingSF gameData)
    where
        sf = proc (appInput, gameSession) -> do
            let nextGameData = setGamePlayer_ (setGameSession_ gameData0 gameSession) gamePlayer0

            -- check events
            playerFallingEvent <- tagWith gameData0 ^<< edge -< checkPlayerStartFalling nextGameData
            playerJumpEvent <- tagWith jumpGameData ^<< jumpTrigger -< appInput
            let playerInTheAirEvent = lMerge playerFallingEvent playerJumpEvent

            returnA -< (gamePlayer0, playerInTheAirEvent)

        gamePlayer0 = gPlayer $ gSession gameData0
        jumpGameData = setGamePlayer_ gameData0 jumpGamePlayer
        jumpGamePlayer = gamePlayer0 { pV = jumpSpeed }
        jumpSpeed = gJumpSpeed $ gSettings gameData0


playerFallingSF :: GameData -> SF (AppInput, GameSession) GamePlayer
playerFallingSF gameData0 = switch sf (\gameData -> playerDrivingSF gameData)
    where
        sf = proc (_, gameSession) -> do
            let nextGameData = setGameSession_ gameData0 gameSession
            nextPlayer <- fallPlayer gamePlayer0 -< pAccelerationY gamePlayer0

            -- check event
            playerDrivingEvent <- edge -< checkPlayerStopFalling $ setGamePlayer_ nextGameData nextPlayer

            returnA -< (nextPlayer, playerDrivingEvent `tag` setGamePlayer_ nextGameData (ajustPlayer nextPlayer))
            where
                ajustPlayer player = player { pPosY = (fromInteger . ceiling $ (pPosY player)) , pV = 0}

        gamePlayer0 = gPlayer $ gSession gameData0


-- Player Functions
checkPlayerStartFalling :: GameData -> Bool
checkPlayerStartFalling gameData = not $ oDrivable $ getPlayerGameObject gameData 0 (-1)

checkPlayerStopFalling :: GameData -> Bool
checkPlayerStopFalling gameData = (oDrivable bottomObj || oDrivable bottomFrontObj)
    where
        bottomObj = getPlayerGameObject gameData 0 0
        bottomFrontObj = getPlayerGameObject gameData 1 0

isPlayerInTheAir :: GameData -> Bool
isPlayerInTheAir gameData = playerY `mod'` playerHeight > 0
        where
            playerY = (pPosY $ gPlayer $ gSession gameData)
            playerHeight = 1

getPlayerGameObject :: GameData -> Int -> Int -> GameObject
getPlayerGameObject gameData offsetX offsetY = oObjects gameColumn !! (gamePlayerY + offsetY)
    where
        gameColumn = gameLevel !! (gamePosX + gamePlayerX + offsetX)
        gamePosX = floor $ gPosX gameSession
        gamePlayer = gPlayer gameSession
        gamePlayerX = floor $ pPosX gamePlayer
        gamePlayerY = floor $ pPosY gamePlayer
        gameSession = gSession gameData
        gameLevel = currentGameLevel gameData


-- Player SF
fallPlayer :: GamePlayer -> SF Double GamePlayer
fallPlayer gamePlayer0 = proc accelerationY -> do
    nextV <- imIntegral playerV0 -< -accelerationY
    nextY <- imIntegral playerPosY0 -< nextV
    returnA -< gamePlayer0 { pV = nextV
                           , pPosY = nextY
                           }
    where
        playerV0 = pV gamePlayer0
        playerPosY0 = pPosY gamePlayer0
