module Main where

import           Board.Board (board, Board, Stone(O, X)
                            , isBoardFull, isThereAWinner)
import           User.Actions.AddStone (addStone)

import           System.Random (randomRIO)
import Data.Char (isDigit)

getUserChoiceInt :: String -> String -> IO Int
getUserChoiceInt name rowOrColumn =
  putStrLn (name ++ " which " ++ rowOrColumn ++" do you want to place your stone?" )
  >> getLine
  >>= (\ch -> let input = head ch
              in  if isDigit input
                  then return $ read @Int [input]
                  else getUserChoiceInt name rowOrColumn
      )

playGame :: (String, Stone) -> Board -> IO Board
playGame p@(name, s) b = do
  row     <-  if isPlayerHumanoid name
              then getUserChoiceInt name "row"
              else randomRIO (1, 3)
  column  <- if isPlayerHumanoid name
              then getUserChoiceInt name "column"
              else randomRIO (1, 3)
  case addStone s (row, column) b of
    Left msg -> putStrLn msg >> playGame p b
    Right nb -> print nb >> putStrLn "" >> return nb

gameLoop :: (String, Stone, Bool) -> (String, Stone, Bool) -> Board -> IO ()
gameLoop (name1, _, active1) (name2, _, _) b
  | isThereAWinner b = if active1
                       then putStrLn (getPlayerName name2 ++ " is a winner!") >> main
                       else putStrLn (getPlayerName name1 ++ " is a winner!") >> main

gameLoop _ _ b
  | isBoardFull b = putStrLn "Board is full. Game over!!" >> main

gameLoop (name1, s1, True) (name2, s2, _) b =
  playGame (name1, s1)  b
  >>= gameLoop (name1, s1, False) (name2, s2, True)

gameLoop (name1, s1, _) (name2, s2, True) b =
  playGame (name2, s2) b
  >>= gameLoop (name1, s1, True) (name2, s2, False)

gameLoop p1 p2 b = gameLoop p1 p2 b

isPlayerHumanoid :: String -> Bool
isPlayerHumanoid "" = False
isPlayerHumanoid _  = True

getPlayerName :: String -> String
getPlayerName name =  if isPlayerHumanoid name
                      then name
                      else "AI"

main :: IO ()
main = do
  putStrLn "Welcome to Tic Tac Toe!"
  putStrLn "Player 1, please enter your name, or keep empty for AI:"
  player1 <- getLine
  putStrLn "Player 2, please enter your name, or keep empty for AI:"
  player2 <- getLine
  putStrLn $ "Player 1: " ++ getPlayerName player1 ++ " vs Player 2: " ++ getPlayerName player2
  gameLoop (player1, O, True) (player2, X,  False) board
