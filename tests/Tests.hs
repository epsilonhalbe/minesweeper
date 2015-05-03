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
  [--scprop
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [--qcprop
  ]

unitTests = testGroup "Unit tests"
  [ testCase "empty board" $
      M.null (get $ readGameBoard gameBoard0) @?= True
  , testCase "single bomb" $ readGameBoard gameBoard1 @?= (GameBoard $ M.fromList $ zipWith [(2,2)]) bombs

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

bombs :: [Bomb]
bombs = repeat Bomb

gameBoard0 :: String
gameBoard0  = unlines ["---" ,"---" ,"---"]

gameBoard1 :: String
gameBoard1  = unlines ["---" ,"-x-" ,"---"]

gameBoard21, gameBoard22, gameBoard23 :: String; gameBoard21', gameBoard22', gameBoard23' :: GameBoard
gameBoard21 = unlines ["xx-" ,"---" ,"---"]; gameBoard21' = GameBoard $ M.fromList $ zip [(1,1),(1,2)] bombs
gameBoard22 = unlines ["x--" ,"x--" ,"---"]; gameBoard22' = GameBoard $ M.fromList $ zip [(1,1),(2,1)] bombs
gameBoard23 = unlines ["x--" ,"-x-" ,"---"]; gameBoard23' = GameBoard $ M.fromList $ zip [(1,1),(2,2)] bombs

gameBoard31, gameBoard32, gameBoard33 :: String
gameBoard31 = unlines ["xxx" ,"---" ,"---"]; gameBoard31' = GameBoard $ M.fromList $ zip [(1,1),(1,2),(1,3)] bombs
gameBoard32 = unlines ["x--" ,"x--" ,"x--"]; gameBoard32' = GameBoard $ M.fromList $ zip [(1,1),(2,1),(3,1)] bombs
gameBoard33 = unlines ["xx-" ,"x--" ,"---"]; gameBoard33' = GameBoard $ M.fromList $ zip [(1,1),(1,2),(2,1)] bombs

gameBoard41, gameBoard42, gameBoard43, gameBoard44 :: String
gameBoard41 = unlines ["x-x" ,"x--" ,"x--"]; gameBoard41' = GameBoard $ M.fromList $ zip [(1,1),(1,3),(2,1),(3,1)] bombs
gameBoard42 = unlines ["x-x" ,"---" ,"x-x"]; gameBoard42' = GameBoard $ M.fromList $ zip [(1,1),(1,3),(3,1),(3,3)] bombs
gameBoard43 = unlines ["x--" ,"x--" ,"x-x"]; gameBoard43' = GameBoard $ M.fromList $ zip [(1,1),(2,1),(3,1),(3,3)] bombs
gameBoard44 = unlines ["xx-" ,"---" ,"-xx"]; gameBoard44' = GameBoard $ M.fromList $ zip [(1,1),(1,2),(3,2),(3,3)] bombs

gameBoard51, gameBoard52, gameBoard53, gameBoard54, gameBoard55 :: String
gameBoard51 = unlines ["x-x" ,"---" ,"xxx"]; gameBoard51' = GameBoard $ M.fromList $ zip [(1,1),(1,3),(3,1),(3,2),(3,3)] bombs
gameBoard52 = unlines ["-x-" ,"x-x" ,"x-x"]; gameBoard52' = GameBoard $ M.fromList $ zip [(1,2),(2,1),(2,3),(3,1),(3,3)] bombs
gameBoard53 = unlines ["xx-" ,"x--" ,"xx-"]; gameBoard53' = GameBoard $ M.fromList $ zip [(1,1),(1,2),(2,1),(3,1),(3,2)] bombs
gameBoard54 = unlines ["-x-" ,"x-x" ,"-xx"]; gameBoard54' = GameBoard $ M.fromList $ zip [(1,2),(2,1),(2,3),(3,2),(3,3)] bombs
gameBoard55 = unlines ["xx-" ,"---" ,"xxx"]; gameBoard55' = GameBoard $ M.fromList $ zip [(1,1),(1,2),(3,1),(3,2),(3,3)] bombs

gameBoard61, gameBoard62, gameBoard63, gameBoard64 :: String
gameBoard65, gameBoard66, gameBoard67, gameBoard68, gameBoard69 :: String
gameBoard61 = unlines ["xxx" ,"---" ,"xxx"]; gameBoard61' = GameBoard $ M.fromList $ zip [(1,1),(1,2),(1,3),(3,1),(3,2),(3,3)] bombs
gameBoard62 = unlines ["--x" ,"x-x" ,"xxx"]; gameBoard62' = GameBoard $ M.fromList $ zip [(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)] bombs
gameBoard63 = unlines ["-x-" ,"x-x" ,"xxx"]; gameBoard63' = GameBoard $ M.fromList $ zip [(1,2),(2,1),(2,3),(3,1),(3,2),(3,3)] bombs
gameBoard64 = unlines ["-xx" ,"x--" ,"xxx"]; gameBoard64' = GameBoard $ M.fromList $ zip [(1,2),(1,3),(2,1),(3,1),(3,2),(3,3)] bombs
gameBoard65 = unlines ["-xx" ,"x-x" ,"xx-"]; gameBoard65' = GameBoard $ M.fromList $ zip [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)] bombs
gameBoard66 = unlines ["x--" ,"x-x" ,"xxx"]; gameBoard66' = GameBoard $ M.fromList $ zip [(1,1),(2,1),(2,3),(3,1),(3,2),(3,3)] bombs
gameBoard67 = unlines ["x-x" ,"x--" ,"xxx"]; gameBoard67' = GameBoard $ M.fromList $ zip [(1,1),(1,3),(2,1),(3,1),(3,2),(3,3)] bombs
gameBoard68 = unlines ["x-x" ,"x-x" ,"xx-"]; gameBoard68' = GameBoard $ M.fromList $ zip [(1,1),(1,3),(2,1),(2,3),(3,1),(3,2)] bombs
gameBoard69 = unlines ["x-x" ,"x-x" ,"x-x"]; gameBoard69' = GameBoard $ M.fromList $ zip [(1,1),(1,3),(2,1),(2,3),(3,1),(3,3)] bombs

gameBoard71, gameBoard72, gameBoard73 :: String
gameBoard71 = unlines ["-xx" ,"x-x" ,"xxx"]
gameBoard71' = GameBoard $ M.fromList $ zip [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)] bombs
gameBoard72 = unlines ["x-x" ,"x-x" ,"xxx"]
gameBoard72' = GameBoard $ M.fromList $ zip [(1,1),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)] bombs
gameBoard73 = unlines ["xx-" ,"x-x" ,"xxx"]
gameBoard73' = GameBoard $ M.fromList $ zip [(1,1),(1,2),(2,1),(2,3),(3,1),(3,2),(3,3)] bombs

gameBoard8 :: String
gameBoard8  = unlines ["xxx" ,"x-x" ,"xxx"]
gameBoard8' = GameBoard $ M.fromList $ zip [(1,1),(1,2),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)] bombs

gameBoards :: [String]
gameBoards =
  [ gameBoard1  , gameBoard21 , gameBoard22 , gameBoard23 , gameBoard31
  , gameBoard32 , gameBoard33 , gameBoard41 , gameBoard42 , gameBoard43
  , gameBoard44 , gameBoard51 , gameBoard52 , gameBoard53 , gameBoard54
  , gameBoard55 , gameBoard61 , gameBoard62 , gameBoard63 , gameBoard64
  , gameBoard65 , gameBoard66 , gameBoard67 , gameBoard68 , gameBoard69
  , gameBoard71 , gameBoard72 , gameBoard73 , gameBoard8 ]
