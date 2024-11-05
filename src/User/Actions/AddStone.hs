module User.Actions.AddStone (addStone) where

import Board.Board (Board(Board), Stone, BoardLine(Line))
import Data.Functor ((<&>))

addStoneLine :: Stone -> Int -> BoardLine -> Either String BoardLine
addStoneLine s 1 (Line Nothing s2 s3) = Right $ Line (Just s) s2 s3
addStoneLine s 2 (Line s1 Nothing s3) = Right $ Line s1 (Just s) s3
addStoneLine s 3 (Line s1 s2 Nothing) = Right $ Line s1 s2 (Just s)
addStoneLine _ n _ | n > 3            = Left $ "Invalid column number. Choose between 1 and 3. You chose: " ++ show n
addStoneLine _ _ _                    = Left "This cell is not empty! Choose another one."

addStone :: Stone -> (Int, Int) -> Board -> Either String Board
addStone s (1, y) (Board l1 l2 l3) = addStoneLine s y l1 <&> (\l -> Board l l2 l3)
-- addStone s (1, y) (Board l1 l2 l3)  = (\l -> Board l l2 l3) <$> addStoneLine s y l1
addStone s (2, y) (Board l1 l2 l3) = addStoneLine s y l2 <&> (\l -> Board l1 l l3)
-- addStone s (2, y) (Board l1 l2 l3)  = (\l -> Board l1 l l3) <$> addStoneLine s y l2
addStone s (3, y) (Board l1 l2 l3) = addStoneLine s y l3 <&> Board l1 l2
-- addStone s (3, y) (Board l1 l2 l3)  = Board l1 l2 <$> addStoneLine s y l3
addStone _ _ _                      = Left "Invalid row number. Choose between 1 and 3."
