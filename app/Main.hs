module Main where

import qualified System.Random as R
import System.IO
import System.Console.ANSI
import Control.Monad (forever, when)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception (finally)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- Game Types
data Direction = North | South | East | West
    deriving (Show, Eq)

data Command = Quit | Go Direction 
    deriving (Show, Eq)

type Position = (Int, Int)
type Snake = [Position]

data PowerUpType = ExtraFood
    deriving (Show, Eq, Enum)

data ChallengeType = SpeedUp
    deriving (Show, Eq, Enum)

data PowerUp = PowerUp {
    powerType :: PowerUpType,
    position :: Position,
    remainingTime :: Int
} deriving (Show, Eq)

data Challenge = Challenge {
    challengeType :: ChallengeType,
    challengePosition :: Position,
    challengeRemainingTime :: Int
} deriving (Show, Eq)

data World = World { 
    snake :: Snake,
    food :: Position,
    direction :: Direction,
    rand :: R.StdGen,
    limits :: (Int, Int),
    score :: Int,
    powerUps :: [PowerUp],
    challenges :: [Challenge],
    invincible :: Int,
    speedBoost :: Int,
    gamePaused :: Int,  -- Pause counter for messages
    lastMessage :: String  -- Last power-up/challenge message
} deriving (Show)

data GameState = Playing World | GameOver Int | Paused World String
    deriving (Show)

-- Utility Functions
opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East = West
opposite West = East

move :: Direction -> Position -> Position
move North (r, c) = (r - 1, c)
move South (r, c) = (r + 1, c)
move East (r, c) = (r, c + 1)
move West (r, c) = (r, c - 1)

-- Snake Operations
slither :: Snake -> Direction -> Snake
slither s d = (move d $ head s) : init s

eat :: Snake -> Direction -> Snake
eat s d = (move d $ head s) : s

-- Random Generation
randomPosition :: R.RandomGen g => (Int, Int) -> g -> (Position, g)
randomPosition (maxr, maxc) g =
    let (r, g1) = R.randomR (1, maxr) g
        (c, g2) = R.randomR (1, maxc) g1
    in ((r, c), g2)

randomFreePosition :: R.RandomGen g => (Int, Int) -> g -> Snake -> [Position] -> (Position, g)
randomFreePosition lim g s occupied =
    head $ dropWhile inUse (randomPositions g)
    where 
        inUse (x, _) = x `elem` (s ++ occupied)
        randomPositions h = r : randomPositions g'
            where r@(_, g') = randomPosition lim h

-- Power-up and Challenge System
generatePowerUp :: R.RandomGen g => (Int, Int) -> g -> Snake -> Position -> [Position] -> (PowerUp, g)
generatePowerUp lim g s foodPos challengePositions =
    let (pos, g1) = randomFreePosition lim g s (foodPos : challengePositions)
        -- Only one power-up type now
        powerType = ExtraFood
        duration = 1000  -- Power-ups stay until collected
    in (PowerUp powerType pos duration, g1)

generateChallenge :: R.RandomGen g => (Int, Int) -> g -> Snake -> Position -> [Position] -> (Challenge, g)
generateChallenge lim g s foodPos powerUpPositions =
    let (pos, g1) = randomFreePosition lim g s (foodPos : powerUpPositions)
        -- Only one challenge type
        challengeType = SpeedUp
        duration = 1000  -- Challenges stay until collected
    in (Challenge challengeType pos duration, g1)

-- Spawn power-up/challenge every 50 points, randomized
shouldSpawnItem :: Int -> Bool
shouldSpawnItem score = score > 0 && score `mod` 50 == 0

-- Game Logic
advance :: World -> Direction -> World
advance w newDir
    | gamePaused w > 0 = w { gamePaused = gamePaused w - 1 }  -- Handle pause
    | newDir == opposite (direction w) = advanceInCurrentDirection
    | newHead == food w = eatFood
    | newHead `elem` map position (powerUps w) = collectPowerUp
    | newHead `elem` map challengePosition (challenges w) = collectChallenge
    | otherwise = normalMove
    where 
        actualDir = if newDir == opposite (direction w) then direction w else newDir
        newHead = move actualDir (head $ snake w)
        
        advanceInCurrentDirection = advance w (direction w)
        
        normalMove = w { 
            snake = slither (snake w) actualDir,
            direction = actualDir,
            invincible = max 0 (invincible w - 1),
            speedBoost = max 0 (speedBoost w - 1)
        }
        
        eatFood = 
            let newSnake = eat (snake w) actualDir
                -- Clear existing power-ups and challenges when eating regular food
                clearedPowerUps = []
                clearedChallenges = []
                allOccupiedPositions = []  -- No existing items after clearing
                (newFood, g1) = randomFreePosition (limits w) (rand w) newSnake allOccupiedPositions
                newScore = score w + 10
                shouldSpawn = shouldSpawnItem newScore
                
                -- Much better randomization with multiple mixing steps
                (mix1, g2) = R.randomR (1 :: Int, 10000) g1
                (mix2, g3) = R.randomR (1 :: Int, 10000) g2  
                (mix3, g4) = R.randomR (1 :: Int, 10000) g3
                -- Use the sum of mixes to determine spawn type for better distribution
                spawnType = (mix1 + mix2 + mix3) `mod` 2
                -- spawnType 0 = power-up (?), spawnType 1 = challenge (!)
                
                (newPowerUps, newChallenges, finalGen) = 
                    if shouldSpawn
                    then if spawnType == 0
                         then let (newPowerUp, g5) = generatePowerUp (limits w) g4 newSnake newFood []
                              in ([newPowerUp], [], g5)
                         else let (newChallenge, g5) = generateChallenge (limits w) g4 newSnake newFood []
                              in ([], [newChallenge], g5)
                    else ([], [], g4)
                    
            in w { 
                snake = newSnake,
                direction = actualDir,
                food = newFood,
                rand = finalGen,
                score = newScore,
                powerUps = newPowerUps,
                challenges = newChallenges,
                invincible = max 0 (invincible w - 1),
                speedBoost = max 0 (speedBoost w - 1)
            }
        
        collectPowerUp = 
            let collectedPowerUp = head $ filter (\p -> position p == newHead) (powerUps w)
                remainingPowerUps = filter (\p -> position p /= newHead) (powerUps w)
                newSnake = eat (snake w) actualDir
                -- Power-up gives 10 points
                (bonusScore, message) = (10, "EXTRA FOOD POWER-UP! Bonus points!")
            in w {
                snake = newSnake,
                direction = actualDir,
                powerUps = remainingPowerUps,
                score = score w + bonusScore,
                invincible = max 0 (invincible w - 1),
                gamePaused = 20,  -- Pause for ~4 seconds (20 * 200ms)
                lastMessage = message
            }
            
        collectChallenge = 
            let collectedChallenge = head $ filter (\c -> challengePosition c == newHead) (challenges w)
                remainingChallenges = filter (\c -> challengePosition c /= newHead) (challenges w)
                newSnake = eat (snake w) actualDir
                -- Challenge gives 15 points
                (newSpeedBoost, message) = (120, "SPEED CHALLENGE! Moving faster! (120 moves)")
            in w {
                snake = newSnake,
                direction = actualDir,
                challenges = remainingChallenges,
                score = score w + 15,  -- Challenge bonus
                speedBoost = newSpeedBoost,
                gamePaused = 20,  -- Pause for ~4 seconds
                lastMessage = message
            }

-- Game State Management
toGameState :: World -> GameState
toGameState w
    | gamePaused w > 0 = Paused w (lastMessage w)
    | collision (snake w) = GameOver (score w)
    | any (outside $ limits w) (snake w) = GameOver (score w)
    | otherwise = Playing w
    where
        collision (x:xs) = x `elem` tail xs
        collision [] = False
        outside (maxr, maxc) (r, c) = r < 1 || r > maxr || c < 1 || c > maxc

-- Input Handling
parseCommand :: Char -> Maybe Command
parseCommand c = case c of
    'q' -> Just Quit
    'Q' -> Just Quit
    'w' -> Just $ Go North
    'W' -> Just $ Go North
    'a' -> Just $ Go West
    'A' -> Just $ Go West
    's' -> Just $ Go South
    'S' -> Just $ Go South
    'd' -> Just $ Go East
    'D' -> Just $ Go East
    _   -> Nothing

-- Input thread that continuously reads keyboard input
inputThread :: TVar Direction -> TVar Bool -> IO ()
inputThread dirVar quitVar = forever $ do
    c <- getChar
    case parseCommand c of
        Just Quit -> atomically $ writeTVar quitVar True
        Just (Go newDir) -> atomically $ writeTVar dirVar newDir
        Nothing -> return ()

-- Display Functions
initScreen :: IO ()
initScreen = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen
    hideCursor

draw :: Char -> Position -> IO ()
draw char (row, col) = do
    setCursorPosition row col
    putChar char

drawBorder :: World -> IO ()
drawBorder w = do
    let (r, c) = limits w
    mapM_ (draw '#') [(0, x) | x <- [0..c+1]]
    mapM_ (draw '#') [(r+1, x) | x <- [0..c+1]]
    mapM_ (draw '#') [(x, 0) | x <- [0..r+1]]
    mapM_ (draw '#') [(x, c+1) | x <- [0..r+1]]

-- Clear the status and message areas completely to prevent text collision
clearStatusArea :: World -> IO ()
clearStatusArea w = do
    let (r, c) = limits w
    -- Clear multiple lines to prevent collision
    setCursorPosition (r + 2) 0
    putStr $ replicate (c + 30) ' '  -- Wider clear
    setCursorPosition (r + 3) 0
    putStr $ replicate (c + 30) ' '
    setCursorPosition (r + 4) 0
    putStr $ replicate (c + 30) ' '
    setCursorPosition (r + 5) 0
    putStr $ replicate (c + 30) ' '

-- Optimized rendering
renderWorld :: World -> IO ()
renderWorld w = renderWorldDiff w Nothing

-- Efficient differential rendering
renderWorldDiff :: World -> Maybe World -> IO ()
renderWorldDiff newWorld oldWorldMaybe = do
    -- Clear only old positions that are no longer occupied
    case oldWorldMaybe of
        Just oldWorld -> do
            let oldTail = last (snake oldWorld)
                newPositions = snake newWorld ++ [food newWorld] ++ 
                             map position (powerUps newWorld) ++ 
                             map challengePosition (challenges newWorld)
            when (oldTail `notElem` newPositions) $ 
                draw ' ' oldTail
            
            when (food oldWorld /= food newWorld && food oldWorld `notElem` newPositions) $
                draw ' ' (food oldWorld)
            
            -- Clear old power-ups and challenges that disappeared
            let oldPowerUpPositions = map position (powerUps oldWorld)
                oldChallengePositions = map challengePosition (challenges oldWorld)
                newPowerUpPositions = map position (powerUps newWorld)
                newChallengePositions = map challengePosition (challenges newWorld)
                removedPositions = filter (`notElem` (newPowerUpPositions ++ newChallengePositions)) 
                                 (oldPowerUpPositions ++ oldChallengePositions)
            mapM_ (draw ' ') removedPositions
        Nothing -> return ()
    
    -- Draw new elements
    draw '*' (food newWorld)
    mapM_ (draw 'O') (snake newWorld)
    mapM_ (draw '?' . position) (powerUps newWorld)
    mapM_ (draw '!' . challengePosition) (challenges newWorld)  -- Use ! for challenges
    
    -- Update status efficiently
    updateStatus newWorld oldWorldMaybe

-- Efficient status updates
updateStatus :: World -> Maybe World -> IO ()
updateStatus newWorld oldWorldMaybe = do
    let shouldUpdate = case oldWorldMaybe of
            Nothing -> True
            Just oldWorld -> score newWorld /= score oldWorld ||
                           speedBoost newWorld /= speedBoost oldWorld
    
    when shouldUpdate $ do
        let (r, _) = limits newWorld
            speedText = if speedBoost newWorld > 0 then "FAST (" ++ show (speedBoost newWorld) ++ ")"
                       else "NORMAL"
            statusText = "Score: " ++ show (score newWorld) ++ 
                        " | Speed: " ++ speedText
        
        setCursorPosition (r + 2) 0
        putStr $ replicate 60 ' '  -- Clear wider area first
        setCursorPosition (r + 2) 0
        putStr statusText

-- Enhanced drawUpdate with better text clearing
drawUpdate :: GameState -> Maybe World -> IO ()
drawUpdate (Playing w) oldWorld = renderWorldDiff w oldWorld
drawUpdate (Paused w message) oldWorld = do
    renderWorldDiff w oldWorld
    -- Clear and display pause message properly
    let (r, _) = limits w
    setCursorPosition (r + 3) 0
    putStr $ replicate 60 ' '  -- Clear wider area
    setCursorPosition (r + 3) 0
    putStr message
drawUpdate (GameOver finalScore) _ = do
    let (r, _) = (20, 30)
    
    -- Clear game area
    setCursorPosition 1 1
    mapM_ (\row -> do
        setCursorPosition row 1
        putStr $ replicate 30 ' ') [1..20]
    
    -- Clear status area completely before showing game over
    mapM_ (\row -> do
        setCursorPosition row 0
        putStr $ replicate 70 ' ') [r+2..r+6]
    
    -- Show game over with proper spacing
    setCursorPosition (r + 3) 0
    putStr $ "GAME OVER! Final Score: " ++ show finalScore
    setCursorPosition (r + 4) 0
    putStr "Press any key to exit..."

-- Function to create initial world with much better randomization
createInitialWorld :: IO World
createInitialWorld = do
    currentTime <- getPOSIXTime
    -- Much more aggressive seed mixing
    let timeMicro = round (currentTime * 1000000)
        timeNano = round (currentTime * 1000000000) `mod` 999999
        -- Mix with multiple prime numbers for better distribution
        seed1 = (timeMicro * 31 + timeNano * 37) `mod` 2147483647
        seed2 = (timeMicro * 41 + timeNano * 43) `mod` 2147483647  
        finalSeed = abs (seed1 + seed2 * 47) `mod` 2147483647
        gen = R.mkStdGen finalSeed
        
        -- Extensive warm-up with 10 random calls
        warmUpGens = take 10 $ iterate (\g -> snd $ R.randomR (1 :: Int, 100000) g) gen
        warmedGen = last warmUpGens
        
        initialSnake = [(10, x) | x <- [12..15]]
        (initialFood, newGen) = randomFreePosition (20, 30) warmedGen initialSnake []
    
    return World { 
        snake = initialSnake,
        food = initialFood,
        direction = West,
        rand = newGen,
        limits = (20, 30),
        score = 0,
        powerUps = [],
        challenges = [],
        invincible = 0,
        speedBoost = 0,
        gamePaused = 0,
        lastMessage = ""
    }

-- Main game loop
gameLoop :: TVar Direction -> TVar Bool -> World -> IO ()
gameLoop dirVar quitVar world = gameLoopWithHistory dirVar quitVar world Nothing

gameLoopWithHistory :: TVar Direction -> TVar Bool -> World -> Maybe World -> IO ()
gameLoopWithHistory dirVar quitVar world previousWorld = do
    shouldQuit <- readTVarIO quitVar
    if shouldQuit
        then return ()
        else do
            currentDir <- readTVarIO dirVar
            
            let newWorld = advance world currentDir
                gameState = toGameState newWorld
            
            drawUpdate gameState previousWorld
            hFlush stdout
            
            case gameState of
                GameOver _ -> do
                    getChar
                    let (r, _) = (20, 30)
                    setCursorPosition (r + 5) 0
                    putStr "Thanks for playing MEDUSA EATS!"
                    return ()
                _ -> do
                    let baseDelay = 200000  -- 200ms base delay
                        actualDelay = if speedBoost newWorld > 0 
                                     then baseDelay - 80000  -- Faster: 120ms (was 150ms)
                                     else baseDelay          -- Normal speed: 200ms
                    threadDelay actualDelay
                    gameLoopWithHistory dirVar quitVar newWorld (Just world)

-- Main Function
main :: IO ()
main = do
    initScreen
    
    setCursorPosition 3 5
    putStrLn "=== MEDUSA EATS ==="
    putStrLn ""
    putStrLn "Controls: WASD to move, Q to quit"
    putStrLn "Collect * for points"
    putStrLn ""
    putStrLn "Items (spawn every 50 points):"
    putStrLn "? Power-up:"
    putStrLn "  - Extra Food: Bonus points"
    putStrLn "! Challenge:"
    putStrLn "  - Speed Up: Move faster (120 moves)"
    putStrLn ""
    putStrLn "NOTE: Items disappear if you eat regular food instead!"
    putStrLn "Game pauses briefly when you collect items!"
    putStrLn "Press any key to start..."
    getChar
    
    initialWorld <- createInitialWorld
    
    clearScreen
    drawBorder initialWorld
    renderWorld initialWorld
    hFlush stdout
    
    dirVar <- newTVarIO (direction initialWorld)
    quitVar <- newTVarIO False
    
    inputThreadId <- forkIO $ inputThread dirVar quitVar
    
    finally 
        (gameLoop dirVar quitVar initialWorld)
        (showCursor)