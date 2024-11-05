{-# LANGUAGE InstanceSigs #-}
module Board.Board where

data Stone = O | X deriving (Eq)
data BoardLine = Line (Maybe Stone) (Maybe Stone) (Maybe Stone)
data Board = Board {fst::BoardLine
                  , snd::BoardLine
                  , thr::BoardLine}

instance Show BoardLine where
  show :: BoardLine -> String
  show (Line ms1 ms2 ms3) = toStr ms1 ++ "|" ++ toStr ms2 ++ "|" ++ toStr ms3
    where
      toStr :: Maybe Stone -> String
      toStr (Just s) = show s
      toStr Nothing = "   "

instance Show Board where
  show :: Board -> String
  show (Board l1 l2 l3) =
    show l1 ++ "\n---+---+---\n" ++
    show l2 ++ "\n---+---+---\n" ++
    show l3

instance Show Stone where
  show :: Stone -> String
  show O = " O "
  show X = " X "

board :: Board
board = Board (Line Nothing Nothing Nothing)
              (Line Nothing Nothing Nothing)
              (Line Nothing Nothing Nothing)

isBoardFull :: Board -> Bool
isBoardFull (Board l1 l2 l3) = all isLineFull [l1, l2, l3]
  where
    isLineFull :: BoardLine -> Bool
    isLineFull (Line (Just _) (Just _) (Just _)) = True
    isLineFull _ = False

isThereAWinner :: Board -> Bool
isThereAWinner b = checkLines b || checkColumn b || checkDiagonal b
  where
    checkLines :: Board -> Bool
    checkLines (Board l1 l2 l3) = any checkLine [l1, l2, l3]
      where
        checkLine :: BoardLine -> Bool
        checkLine (Line (Just s1) (Just s2) (Just s3)) | sameStone s1 s2 s3 = True
        checkLine _ = False

    checkColumn :: Board -> Bool
    checkColumn (Board (Line (Just s1) _ _) (Line (Just s2) _ _) (Line (Just s3) _ _))
      | sameStone s1 s2 s3 = True
    checkColumn (Board (Line _ (Just s1) _) (Line _ (Just s2) _) (Line _ (Just s3) _))
      | sameStone s1 s2 s3 = True
    checkColumn (Board (Line _ _ (Just s1)) (Line _ _ (Just s2)) (Line _ _ (Just s3)))
      | sameStone s1 s2 s3 = True
    checkColumn _ = False

    checkDiagonal :: Board -> Bool
    checkDiagonal (Board (Line (Just s1) _ _) (Line _ (Just s2) _) (Line _ _ (Just s3)))
      | sameStone s1 s2 s3 = True
    checkDiagonal (Board (Line _ _ (Just s1)) (Line _ (Just s2) _) (Line (Just s3) _ _))
      | sameStone s1 s2 s3 = True
    checkDiagonal _ = False

    sameStone :: Stone -> Stone -> Stone -> Bool
    sameStone O O O = True
    sameStone X X X = True
    sameStone _ _ _ = False
