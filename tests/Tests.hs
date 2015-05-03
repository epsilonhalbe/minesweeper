import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data
import qualified Data.Map as M

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "neighbours are always 9" $
      \i-> length (neighbours (i :: (Int,Int))) == 9
  --scprop
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [--qcprop
  ]

unitTests = testGroup "Unit tests" [readGameBoardTests, dangerBoardTests]

readGameBoardTests = testGroup "readGameBoards"
  [ testCase "empty board" $
      M.null (get $ readGameBoard gameBoard0) @?= True
  , testCase "single bomb" $ readGameBoard gameBoard1 @?= gameBoard1'

  , testCase "2 bombs horizontal" $ readGameBoard gameBoard21 @?= gameBoard21'
  , testCase "2 bombs vertical"   $ readGameBoard gameBoard22 @?= gameBoard22'
  , testCase "2 bombs diagonal"   $ readGameBoard gameBoard23 @?= gameBoard23'

  , testCase "3 bombs horizontal" $ readGameBoard gameBoard31 @?= gameBoard31'
  , testCase "3 bombs vertical"   $ readGameBoard gameBoard32 @?= gameBoard32'
  , testCase "3 bombs corner"     $ readGameBoard gameBoard33 @?= gameBoard33'

  , testCase ("4 bombs "++ show gameBoard41) $ readGameBoard gameBoard41 @?= gameBoard41'
  , testCase ("4 bombs "++ show gameBoard42) $ readGameBoard gameBoard42 @?= gameBoard42'
  , testCase ("4 bombs "++ show gameBoard43) $ readGameBoard gameBoard43 @?= gameBoard43'
  , testCase ("4 bombs "++ show gameBoard44) $ readGameBoard gameBoard44 @?= gameBoard44'

  , testCase ("5 bombs "++ show gameBoard51) $ readGameBoard gameBoard51 @?= gameBoard51'
  , testCase ("5 bombs "++ show gameBoard52) $ readGameBoard gameBoard52 @?= gameBoard52'
  , testCase ("5 bombs "++ show gameBoard53) $ readGameBoard gameBoard53 @?= gameBoard53'
  , testCase ("5 bombs "++ show gameBoard54) $ readGameBoard gameBoard54 @?= gameBoard54'
  , testCase ("5 bombs "++ show gameBoard55) $ readGameBoard gameBoard55 @?= gameBoard55'

  , testCase ("6 bombs "++ show gameBoard61) $ readGameBoard gameBoard61 @?= gameBoard61'
  , testCase ("6 bombs "++ show gameBoard62) $ readGameBoard gameBoard62 @?= gameBoard62'
  , testCase ("6 bombs "++ show gameBoard63) $ readGameBoard gameBoard63 @?= gameBoard63'
  , testCase ("6 bombs "++ show gameBoard64) $ readGameBoard gameBoard64 @?= gameBoard64'
  , testCase ("6 bombs "++ show gameBoard65) $ readGameBoard gameBoard65 @?= gameBoard65'
  , testCase ("6 bombs "++ show gameBoard66) $ readGameBoard gameBoard66 @?= gameBoard66'
  , testCase ("6 bombs "++ show gameBoard67) $ readGameBoard gameBoard67 @?= gameBoard67'
  , testCase ("6 bombs "++ show gameBoard68) $ readGameBoard gameBoard68 @?= gameBoard68'
  , testCase ("6 bombs "++ show gameBoard69) $ readGameBoard gameBoard69 @?= gameBoard69'

  , testCase ("7 bombs "++ show gameBoard71) $ readGameBoard gameBoard71 @?= gameBoard71'
  , testCase ("7 bombs "++ show gameBoard72) $ readGameBoard gameBoard72 @?= gameBoard72'
  , testCase ("7 bombs "++ show gameBoard73) $ readGameBoard gameBoard73 @?= gameBoard73'

  , testCase ("8 bombs "++ show gameBoard8) $ readGameBoard gameBoard8 @?= gameBoard8'
  --hunit
  ]

dangerBoardTests = testGroup "dangerBoards"
  [ testCase "no bomb" $ (printArray . dangerBoard . readGameBoard) gameBoard0 @?= dangerBoard0
  , testCase "single bomb" $ (printArray . dangerBoard . readGameBoard) gameBoard1 @?= dangerBoard1

  , testCase "2 bombs horizontal" $ (printArray . dangerBoard . readGameBoard) gameBoard21 @?= dangerBoard21

  , testCase "2 bombs vertical"   $ (printArray . dangerBoard . readGameBoard) gameBoard22 @?= dangerBoard22
  , testCase "2 bombs diagonal"   $ (printArray . dangerBoard . readGameBoard) gameBoard23 @?= dangerBoard23

  , testCase "3 bombs horizontal" $ (printArray . dangerBoard . readGameBoard) gameBoard31 @?= dangerBoard31
  , testCase "3 bombs vertical"   $ (printArray . dangerBoard . readGameBoard) gameBoard32 @?= dangerBoard32
  , testCase "3 bombs corner"     $ (printArray . dangerBoard . readGameBoard) gameBoard33 @?= dangerBoard33

  , testCase ("4 bombs "++ show gameBoard41) $ (printArray . dangerBoard . readGameBoard) gameBoard41 @?= dangerBoard41
  , testCase ("4 bombs "++ show gameBoard42) $ (printArray . dangerBoard . readGameBoard) gameBoard42 @?= dangerBoard42
  , testCase ("4 bombs "++ show gameBoard43) $ (printArray . dangerBoard . readGameBoard) gameBoard43 @?= dangerBoard43
  , testCase ("4 bombs "++ show gameBoard44) $ (printArray . dangerBoard . readGameBoard) gameBoard44 @?= dangerBoard44

  , testCase ("5 bombs "++ show gameBoard51) $ (printArray . dangerBoard . readGameBoard) gameBoard51 @?= dangerBoard51
  , testCase ("5 bombs "++ show gameBoard52) $ (printArray . dangerBoard . readGameBoard) gameBoard52 @?= dangerBoard52
  , testCase ("5 bombs "++ show gameBoard53) $ (printArray . dangerBoard . readGameBoard) gameBoard53 @?= dangerBoard53
  , testCase ("5 bombs "++ show gameBoard54) $ (printArray . dangerBoard . readGameBoard) gameBoard54 @?= dangerBoard54
  , testCase ("5 bombs "++ show gameBoard55) $ (printArray . dangerBoard . readGameBoard) gameBoard55 @?= dangerBoard55

  , testCase ("6 bombs "++ show gameBoard61) $ (printArray . dangerBoard . readGameBoard) gameBoard61 @?= dangerBoard61
  , testCase ("6 bombs "++ show gameBoard62) $ (printArray . dangerBoard . readGameBoard) gameBoard62 @?= dangerBoard62
  , testCase ("6 bombs "++ show gameBoard63) $ (printArray . dangerBoard . readGameBoard) gameBoard63 @?= dangerBoard63
  , testCase ("6 bombs "++ show gameBoard64) $ (printArray . dangerBoard . readGameBoard) gameBoard64 @?= dangerBoard64
  , testCase ("6 bombs "++ show gameBoard65) $ (printArray . dangerBoard . readGameBoard) gameBoard65 @?= dangerBoard65
  , testCase ("6 bombs "++ show gameBoard66) $ (printArray . dangerBoard . readGameBoard) gameBoard66 @?= dangerBoard66
  , testCase ("6 bombs "++ show gameBoard67) $ (printArray . dangerBoard . readGameBoard) gameBoard67 @?= dangerBoard67
  , testCase ("6 bombs "++ show gameBoard68) $ (printArray . dangerBoard . readGameBoard) gameBoard68 @?= dangerBoard68
  , testCase ("6 bombs "++ show gameBoard69) $ (printArray . dangerBoard . readGameBoard) gameBoard69 @?= dangerBoard69

  , testCase ("7 bombs "++ show gameBoard71) $ (printArray . dangerBoard . readGameBoard) gameBoard71 @?= dangerBoard71
  , testCase ("7 bombs "++ show gameBoard72) $ (printArray . dangerBoard . readGameBoard) gameBoard72 @?= dangerBoard72
  , testCase ("7 bombs "++ show gameBoard73) $ (printArray . dangerBoard . readGameBoard) gameBoard73 @?= dangerBoard73

  , testCase ("8 bombs "++ show gameBoard8) $ (printArray . dangerBoard . readGameBoard) gameBoard8 @?= dangerBoard8
  --hunit
  ]


bombs :: [Bomb]
bombs = repeat Bomb

gameBoard0 :: String
gameBoard0  = unlines ["---" ,"---" ,"---"]

gameBoard1 :: String
gameBoard1  = unlines ["---" ,"-x-" ,"---"]; gameBoard1' = GameBoard (M.fromList $ zip [(2,2)] bombs) 3 3

gameBoard21, gameBoard22, gameBoard23 :: String; gameBoard21', gameBoard22', gameBoard23' :: GameBoard
gameBoard21 = unlines ["xx-" ,"---" ,"---"]; gameBoard21' = GameBoard (M.fromList $ zip [(1,1),(1,2)] bombs) 3 3
gameBoard22 = unlines ["x--" ,"x--" ,"---"]; gameBoard22' = GameBoard (M.fromList $ zip [(1,1),(2,1)] bombs) 3 3
gameBoard23 = unlines ["x--" ,"-x-" ,"---"]; gameBoard23' = GameBoard (M.fromList $ zip [(1,1),(2,2)] bombs) 3 3

gameBoard31, gameBoard32, gameBoard33 :: String
gameBoard31 = unlines ["xxx" ,"---" ,"---"]; gameBoard31' = GameBoard (M.fromList $ zip [(1,1),(1,2),(1,3)] bombs) 3 3
gameBoard32 = unlines ["x--" ,"x--" ,"x--"]; gameBoard32' = GameBoard (M.fromList $ zip [(1,1),(2,1),(3,1)] bombs) 3 3
gameBoard33 = unlines ["xx-" ,"x--" ,"---"]; gameBoard33' = GameBoard (M.fromList $ zip [(1,1),(1,2),(2,1)] bombs) 3 3

gameBoard41, gameBoard42, gameBoard43, gameBoard44 :: String
gameBoard41 = unlines ["x-x" ,"x--" ,"x--"]; gameBoard41' = GameBoard (M.fromList $ zip [(1,1),(1,3),(2,1),(3,1)] bombs) 3 3
gameBoard42 = unlines ["x-x" ,"---" ,"x-x"]; gameBoard42' = GameBoard (M.fromList $ zip [(1,1),(1,3),(3,1),(3,3)] bombs) 3 3
gameBoard43 = unlines ["x--" ,"x--" ,"x-x"]; gameBoard43' = GameBoard (M.fromList $ zip [(1,1),(2,1),(3,1),(3,3)] bombs) 3 3
gameBoard44 = unlines ["xx-" ,"---" ,"-xx"]; gameBoard44' = GameBoard (M.fromList $ zip [(1,1),(1,2),(3,2),(3,3)] bombs) 3 3

gameBoard51, gameBoard52, gameBoard53, gameBoard54, gameBoard55 :: String
gameBoard51 = unlines ["x-x" ,"---" ,"xxx"]; gameBoard51' = GameBoard (M.fromList $ zip [(1,1),(1,3),(3,1),(3,2),(3,3)] bombs) 3 3
gameBoard52 = unlines ["-x-" ,"x-x" ,"x-x"]; gameBoard52' = GameBoard (M.fromList $ zip [(1,2),(2,1),(2,3),(3,1),(3,3)] bombs) 3 3
gameBoard53 = unlines ["xx-" ,"x--" ,"xx-"]; gameBoard53' = GameBoard (M.fromList $ zip [(1,1),(1,2),(2,1),(3,1),(3,2)] bombs) 3 3
gameBoard54 = unlines ["-x-" ,"x-x" ,"-xx"]; gameBoard54' = GameBoard (M.fromList $ zip [(1,2),(2,1),(2,3),(3,2),(3,3)] bombs) 3 3
gameBoard55 = unlines ["xx-" ,"---" ,"xxx"]; gameBoard55' = GameBoard (M.fromList $ zip [(1,1),(1,2),(3,1),(3,2),(3,3)] bombs) 3 3

gameBoard61, gameBoard62, gameBoard63, gameBoard64 :: String
gameBoard65, gameBoard66, gameBoard67, gameBoard68, gameBoard69 :: String
gameBoard61 = unlines ["xxx" ,"---" ,"xxx"]; gameBoard61' = GameBoard (M.fromList $ zip [(1,1),(1,2),(1,3),(3,1),(3,2),(3,3)] bombs) 3 3
gameBoard62 = unlines ["--x" ,"x-x" ,"xxx"]; gameBoard62' = GameBoard (M.fromList $ zip [(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)] bombs) 3 3
gameBoard63 = unlines ["-x-" ,"x-x" ,"xxx"]; gameBoard63' = GameBoard (M.fromList $ zip [(1,2),(2,1),(2,3),(3,1),(3,2),(3,3)] bombs) 3 3
gameBoard64 = unlines ["-xx" ,"x--" ,"xxx"]; gameBoard64' = GameBoard (M.fromList $ zip [(1,2),(1,3),(2,1),(3,1),(3,2),(3,3)] bombs) 3 3
gameBoard65 = unlines ["-xx" ,"x-x" ,"xx-"]; gameBoard65' = GameBoard (M.fromList $ zip [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)] bombs) 3 3
gameBoard66 = unlines ["x--" ,"x-x" ,"xxx"]; gameBoard66' = GameBoard (M.fromList $ zip [(1,1),(2,1),(2,3),(3,1),(3,2),(3,3)] bombs) 3 3
gameBoard67 = unlines ["x-x" ,"x--" ,"xxx"]; gameBoard67' = GameBoard (M.fromList $ zip [(1,1),(1,3),(2,1),(3,1),(3,2),(3,3)] bombs) 3 3
gameBoard68 = unlines ["x-x" ,"x-x" ,"xx-"]; gameBoard68' = GameBoard (M.fromList $ zip [(1,1),(1,3),(2,1),(2,3),(3,1),(3,2)] bombs) 3 3
gameBoard69 = unlines ["x-x" ,"x-x" ,"x-x"]; gameBoard69' = GameBoard (M.fromList $ zip [(1,1),(1,3),(2,1),(2,3),(3,1),(3,3)] bombs) 3 3

gameBoard71, gameBoard72, gameBoard73 :: String
gameBoard71 = unlines ["-xx" ,"x-x" ,"xxx"]
gameBoard71' = GameBoard (M.fromList $ zip [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)] bombs) 3 3
gameBoard72 = unlines ["x-x" ,"x-x" ,"xxx"]
gameBoard72' = GameBoard (M.fromList $ zip [(1,1),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)] bombs) 3 3
gameBoard73 = unlines ["xx-" ,"x-x" ,"xxx"]
gameBoard73' = GameBoard (M.fromList $ zip [(1,1),(1,2),(2,1),(2,3),(3,1),(3,2),(3,3)] bombs) 3 3

gameBoard8 :: String
gameBoard8  = unlines ["xxx" ,"x-x" ,"xxx"]
gameBoard8' = GameBoard (M.fromList $ zip [(1,1),(1,2),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)] bombs) 3 3

dangerBoard0  = unlines ["0 0 0" ,"0 0 0" ,"0 0 0"]

dangerBoard1  = unlines ["1 1 1" ,"1 B 1" ,"1 1 1"]

dangerBoard21 = unlines ["B B 1" ,"2 2 1" ,"0 0 0"]
dangerBoard22 = unlines ["B 2 0" ,"B 2 0" ,"1 1 0"]
dangerBoard23 = unlines ["B 2 1" ,"2 B 1" ,"1 1 1"]

dangerBoard31 = unlines ["B B B" ,"2 3 2" ,"0 0 0"]
dangerBoard32 = unlines ["B 2 0" ,"B 3 0" ,"B 2 0"]
dangerBoard33 = unlines ["B B 1" ,"B 3 1" ,"1 1 0"]

dangerBoard41 = unlines ["B 3 B" ,"B 4 1" ,"B 2 0"]
dangerBoard42 = unlines ["B 2 B" ,"2 4 2" ,"B 2 B"]
dangerBoard43 = unlines ["B 2 0" ,"B 4 1" ,"B 3 B"]
dangerBoard44 = unlines ["B B 1" ,"3 4 3" ,"1 B B"]

dangerBoard51 = unlines ["B 2 B" ,"3 5 3" ,"B B B"]
dangerBoard52 = unlines ["2 B 2" ,"B 5 B" ,"B 4 B"]
dangerBoard53 = unlines ["B B 1" ,"B 5 2" ,"B B 1"]
dangerBoard54 = unlines ["2 B 2" ,"B 5 B" ,"2 B B"]
dangerBoard55 = unlines ["B B 1" ,"4 5 3" ,"B B B"]

dangerBoard61 = unlines ["B B B" ,"4 6 4" ,"B B B"]
dangerBoard62 = unlines ["1 3 B" ,"B 6 B" ,"B B B"]
dangerBoard63 = unlines ["2 B 2" ,"B 6 B" ,"B B B"]
dangerBoard64 = unlines ["2 B B" ,"B 6 4" ,"B B B"]
dangerBoard65 = unlines ["2 B B" ,"B 6 B" ,"B B 2"]
dangerBoard66 = unlines ["B 3 1" ,"B 6 B" ,"B B B"]
dangerBoard67 = unlines ["B 3 B" ,"B 6 3" ,"B B B"]
dangerBoard68 = unlines ["B 4 B" ,"B 6 B" ,"B B 2"]
dangerBoard69 = unlines ["B 4 B" ,"B 6 B" ,"B 4 B"]

dangerBoard71 = unlines ["2 B B" ,"B 7 B" ,"B B B"]
dangerBoard72 = unlines ["B 4 B" ,"B 7 B" ,"B B B"]
dangerBoard73 = unlines ["B B 2" ,"B 7 B" ,"B B B"]

dangerBoard8  = unlines ["B B B" ,"B 8 B" ,"B B B"]

