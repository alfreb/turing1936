module Turing1936Spec (spec, testmain) where

import Test.Hspec
import Test.HUnit

import Turing1936


preamble = do
  putStrLn "Testing Turing's Machines"


completeConfig1 :: CompleteConfiguration
completeConfig1 = ('𝔞', 6, "Clean Machine")

completeConfig2 :: CompleteConfiguration
completeConfig2 = ('𝔟', 7, "Turing's Machine")

tmrow1 :: TuringMachineRow
tmrow1 = (𝔟, (==none), [_P0],  𝔟)

tmrow2 :: TuringMachineRow
tmrow2 = (𝔟, (=='0'), [_R, _R, _P1],  𝔟)

tmrow3 :: TuringMachineRow
tmrow3 = (𝔟, (=='1'), [_R, _R, _P0],  𝔟)

tm1init :: CompleteConfiguration
tm1init = (𝔟, 0, take 50 $ repeat ' ')

tm2init :: CompleteConfiguration
tm2init = (𝔟, 0, take 100 $ repeat ' ')


tmloop1 :: TuringMachine
tmloop1 = [(𝔟, (\x -> True), [], 𝔟)]

tmloop1init :: CompleteConfiguration
tmloop1init = (𝔟, 0, " ")

spec :: Spec
spec = do
  describe "Turing Machines" $ do 
    describe "m-config" $ do
      it "represents one of a finite number of conditions in a program" $ do
        mconfig('𝔷',0,"") `shouldSatisfy` isMconfig 
        
      it "is the first element in a _complete configuration_" $ do
        mconfig completeConfig1 `shouldBe` '𝔞'
        mconfig completeConfig2 `shouldBe` '𝔟'

      it "is the first element in a _configuration_" $ do
        let (m, _) = config completeConfig1 in
          m `shouldSatisfy` isMconfig
          
        
    describe "complete configuration" $ do 
      it "is an m-config, the number of the scanned square and a tape" $ do
        let (m, _, _) = completeConfig1 in
          m `shouldSatisfy` isMconfig
        let (_,n, t) = completeConfig1 in
          t !! n `shouldSatisfy` isSymbol

    describe "configuration" $ do 
      it "is an m-config and the scanned symbol" $ do
        let (m, _) = config completeConfig1 in
          m `shouldSatisfy` isMconfig
        let (_, i) = config completeConfig1 in
          i `shouldSatisfy` isSymbol
          
      it "can be fetched from a complete configuration" $ do
        config completeConfig1 `shouldBe` ('𝔞', 'M')
        config completeConfig2 `shouldBe` ('𝔟', 's')
        
    describe "Operation R" $ do
      it "scans the symbol to the right" $ do
        config (_R completeConfig1) `shouldBe` ('𝔞', 'a')
        config (_R completeConfig2) `shouldBe` ('𝔟', ' ')
        
    describe "Operation L" $ do
      it "scans the symbol to the left" $ do
        config (_L completeConfig1) `shouldBe` ('𝔞', ' ')

    describe "Operation P0" $ do
      it "puts '0' on the tape at the current position" $ do
        tape (_P0 completeConfig1) `shouldBe` "Clean 0achine"
        tape (_P0 completeConfig2) `shouldBe` "Turing'0 Machine"

    describe "Operation P0" $ do
      it "puts '1' on the tape at the current position" $ do
        tape (_P1 completeConfig1) `shouldBe` "Clean 1achine"
        tape (_P1 completeConfig2) `shouldBe` "Turing'1 Machine"
                
    describe "A Turing Machine Row" $ do 
      it "Is a four-tuple that starts with an m-config" $ do
        let (m,_,_,_)  = tmrow1 in
          m `shouldSatisfy` isMconfig
          
      it "The second element is a symbol predicate" $ do
        let (_,sp,_,_) = tmrow1 in
          assertBool "Impossible" ((\x -> sp x `elem` [True, False]) 'x')
          
      it "The third element is a list of operations" $ do
        let (_,_,ops,_) = tmrow1 in
          let op = ops !! 0 in
            config (op completeConfig1) `shouldBe` ('𝔞', '0')
            
      it "The fourth element is the name of the next m-config" $ do
        let (_,_,_,f) = tmrow1 in
          f `shouldSatisfy` isMconfig
      it "The right row is found by searching for one matching our config" $ do
        configMatch ('𝔟', ' ') tmrow1 `shouldBe` True

    describe "Moves of the machine" $ do
      it "A 'move' applies a set of operations to a complete configuration" $ do
        let (m, n, t) = apply (operations tmrow2) completeConfig1 in
          t `shouldBe` "Clean Ma1hine"
        tape (move tm1 tm1init) `shouldBe` "0" ++ (take 49 $ repeat ' ')

      it "'moves' executes n successive moves" $ do
        (moves 20 tm1 tm1init) `shouldBe`
          ('𝔟',38,"0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1           ")
      
    describe "Turing's first example" $ do
      it "The first row has one operation, simply P0, e.g. \"print '0'\"" $ do
        (tape $ apply (operations tmrow1) (𝔟,0," ")) `shouldBe` "0"
        (tape $ apply (operations tmrow1) completeConfig1) `shouldBe` "Clean 0achine"
        
      it "Produces the sequence 010101... with spaces in between" $ do
        (condense $ tape $ moves 20 tm1 tm1init) `shouldBe` "01010101010101010101"    
    
    describe "Turing's second example" $ do
      it "The first row initializes the tape with \"әә0 0\"" $ do
        take 5 (tape $ move tm2 tm1init) `shouldBe` "әә0 0"

      it "Produces the sequence 001011011101111011111... with spaces" $ do
        (drop 2 $ condense $ tape $ moves 240 tm2 tm2init)
          `shouldBe` "001011011101111011111"

    describe "A Turing machine can have infinite loops that do nothing" $ do
      it "If A row has m-config identical to final m-config for any or no symbol" $ do
        moves 300 tmloop1 tmloop1init `shouldBe` tmloop1init
        
    
testmain:: IO ()
testmain = hspec spec

tm = testmain
