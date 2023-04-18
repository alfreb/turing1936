{-|

An implementation of Turing Machines from Turing's 1936 paper
"On computable numbers, with an application to the Entscheidungsproblem."
Proceedings of the London Mathematical Society, Ser. 2, Vol. 42, 1937.

The reference paper can be found in The Turing Digital Archive, Kings College,
Cambridge who provides an online version available here:

https://turingarchive.kings.cam.ac.uk/publications-lectures-and-talks-amtb/amt-b-12

Copyright © London Mathematical Society 1937

-}

module Turing1936 (
  MConfiguration, Tape, TapeIndex, Configuration,
  TuringMachineTable,TuringMachineRow,
  Table, TuringMachine (..),
  CompleteConfiguration, CompleteConfig, getTape, cc,
  Operation(..), perform, _Any, none,
  mconfig, config,
  apply, operations, move, moves,
  condense, getRow,
  configMatch, SymbolPredicate (..), sym,
  prettyConfig, printSteps,
  𝔞,𝔟,𝔠,𝔣,𝔬,𝔭,𝔮,
  isMconfig,isSymbol,isNumScannedSquare,
  tm1, tm2
  )  where

{-|

"We may compare a man in the process of computing a real number to a machine
which is only capable of a finite number of conditions q1: q2. .... qR; which
will be called \"m-configurations\""

p. 2 (231)
-}
type MConfiguration        = Char
type MConfig               = MConfiguration


mconfig :: CompleteConfiguration -> MConfiguration
mconfig (m, _, _) = m


{-| "The machine is supplied with a "tape" (the analogue of paper) running
through it, and divided into sections (called "squares") each capable of bearing
a "symbol". At any moment there is just one square, say the r-th, bearing the
symbol 𝔖 (r) which is \"in the machine\""

p. 2 (231)
-}
type Tape                  = [Char]
type Symbol                = Char

getTape :: CompleteConfiguration -> Tape
getTape (_, _, t) = t

{-| "At any stage of the motion of the machine, the number of the scanned square,
the complete sequence of all symbols on the tape, and the m-configuration will
be said to describe the _complete configuration_ at that stage." -}
type NumScannedSquare      = Int
type TapeIndex             = NumScannedSquare -- TapeIndex is more intuitive
type CompleteConfiguration = (MConfig, NumScannedSquare, Tape)
type CompleteConfig = CompleteConfiguration
-- Note that when defining a complete configuration we orefer to follow the same
-- order as in Turing's example machines, rather than the order in which he
-- lists them in text. There m-config comes first, then symbol, for which we
-- only get the "number" in the "complete configuration"

scannedSymbol :: CompleteConfiguration -> Symbol
scannedSymbol (_, n, tape) = tape !! n

{-| "The possible behaviour of the machine at any moment is determined by the
m-configuration qn and the scanned symbol 𝔖 (r). This pair qn, 𝔖 (r) will be
called the "configuration": thus the configuration determines the possible
behaviour of the machine."

p. 2 (231)
-}
type Configuration = (MConfiguration, Symbol)
type Config = Configuration
configuration :: CompleteConfiguration -> Configuration
-- NOTE: Fetching the configuration implies address lookup, where r is the tape
--       index and 𝔖 (r) is the symbol found at r.

{-| "The changes of the machine and tape between successive complete
configurations will be called the /moves/ of the machine." -}
move :: TuringMachine -> TuringMachine

data Operation = L | R
               | P0 | P1 | Pә | Px
               | P Char
               | E
               deriving (Show, Eq)

type Op                    = CompleteConfiguration -> CompleteConfiguration
type Operations            = [Operation]

{-|

The symbol predicates used by Turing are defined implicitly and via exampls.
Some relevant mentions are:

"The "scanned symbol" is the only one of which the machine is, so to speak,
"directly aware"."

..

"The possible behaviour of the machine at any moment is determined by the
m-configuration qn and the scanned symbol 𝔖 (r).


(p. 231)

"Some of the symbols written down will form the sequence of figures which is the
decimal of the real number which is being computed. The others are just rough
notes to "assist the memory ".
(p. 232)

"When the second column is left blank, it is understood that the behaviour of
the third and fourth columns applies for any symbol and for no symbol."

(p. 233)
-}
data SymbolPredicate       = Is Char
                           | AnyOf String
                           | Any | None
                           | All
                           deriving (Show, Eq)

type TuringMachineRow      = (MConfig, SymbolPredicate, Operations, MConfig)
type TuringMachineTable    = [TuringMachineRow]
type Table                 = TuringMachineTable

data TuringMachine = TM {
  position :: NumScannedSquare,
  tape     :: Tape,
  m_config :: MConfig,
  table    :: TuringMachineTable,
  comments :: String
} deriving (Show, Eq)


sym :: SymbolPredicate -> Symbol -> Bool
sym (Is c)    = (==c)
sym (AnyOf s) = (`elem` s)
sym Any       = (/= ' ')
sym None      = (==' ')
sym All       = (\x -> True)


operations :: TuringMachineRow -> Operations
operations (_,_,ops,_) = ops

finalMconfig :: TuringMachineRow -> MConfig
finalMconfig (_,_,_,f) = f


𝔞='𝔞'; 𝔟='𝔟'; 𝔠='𝔠'; 𝔡='𝔡'; 𝔢='𝔢'; 𝔣='𝔣'; 𝔤='𝔤'; 𝔥='𝔥'; 𝔦='𝔦'; 𝔧='𝔧'; 𝔨='𝔨';
𝔩='𝔩'; 𝔪='𝔪'; 𝔫='𝔫'; 𝔬='𝔬'; 𝔭='𝔭'; 𝔮='𝔮'; 𝔯='𝔯'; 𝔰='𝔰'; 𝔱='𝔱'; 𝔲='𝔲'; 𝔳='𝔳';
𝔴='𝔴'; 𝔵='𝔵'; 𝔶='𝔶'; 𝔷='𝔷';

gothicFraktur = [𝔞,𝔟,𝔠,𝔡,𝔢,𝔣,𝔤,𝔥,𝔦,𝔧,𝔨,𝔩,𝔪,𝔫,𝔬,𝔭,𝔮,𝔯,𝔰,𝔱,𝔲,𝔳,𝔴,𝔵,𝔶,𝔷]
mconfigNames  = gothicFraktur

_R :: Op
_R (m, n, t) = let t' = if n < length t then t else t ++ "  "
               in (m, n + 1, t')

_L :: Op
_L (m, n, t) = (m, n - 1, t)

put c t n = (take n t ++ [c] ++ drop (n + 1) t)

_P :: Symbol -> CompleteConfiguration -> CompleteConfiguration
_P s (m, n, t) = (m, n, put s t n)


perform :: Operation -> CompleteConfiguration -> CompleteConfiguration
perform L  = _L
perform R  = _R
perform P0 = _P '0'
perform P1 = _P '1'
perform Px = _P 'x'
perform Pә = _P 'ә'
perform E  = _P ' '

perform (P x) = (_P x)


configuration (m, n, tape) = (m, tape !! n)
config = configuration

configMatch :: Configuration -> TuringMachineRow -> Bool
configMatch (m, s) (m', sp, _, _) = m == m' && sym sp s

getRow :: TuringMachineTable -> Configuration -> TuringMachineRow
getRow (r:rs) c = if configMatch c r then r
                  else getRow rs c

apply :: [Operation] -> CompleteConfiguration -> CompleteConfiguration
apply [] c = c
apply (op:ops) c = apply ops (perform op c)

move (TM n tape mconf table s) =
  let (m, sp, ops, f) = getRow table $ config (mconf, n, tape) in
    let (mconf', n', tape') = apply ops (mconf, n, tape) in
      (TM n' tape' f table s)


moves :: Int -> TuringMachine -> TuringMachine
moves 0 tm = tm
moves n tm = moves (n-1) (move tm)

cc :: TuringMachine -> CompleteConfiguration
cc (TM n tape mc _ _)  = (mc, n, tape)

step :: Int -> TuringMachine -> [CompleteConfiguration]
step 0 tm = []
step n tm = let tm' = move tm in
              cc tm' : (step (n-1) tm')

stripr [] = []
stripr s = if last s == ' ' then stripr $ init s else s

highlightTape n t = (take n $ repeat ' ') ++ "^"

prettyTape n tp = let t = stripr tp in
                    (take n t) ++ "[" ++ [(t !! n)] ++ "]" ++ (drop (n+1) t) ++
                    "\n" ++ highlightTape n t


prettyConfig :: CompleteConfiguration ->  String
prettyConfig (m, n, t) =
  --"\nIdx: " ++ (show n) ++ " scanned: '" ++ [(t !! n)] ++ "'\n" ++
  [m] ++ " : " ++ (stripr t)  ++
  "\n    " ++ highlightTape n t


printConfigs s = mapM_ (putStrLn . prettyConfig) s

printSteps n tm = printConfigs (step n tm)

none = ' '


isMconfig :: MConfig -> Bool
isMconfig x = x `elem` mconfigNames

isNumScannedSquare :: TapeIndex -> Bool
isNumScannedSquare x = True

condense :: Tape -> Tape
condense []     = []
condense (s:ss) = if s == none then condense ss else s:(condense ss)

{-|  "A machine can be constructed to compute the sequence 010101... .
The machine is to have the four m-configurations "𝔟", "𝔠", "𝔨", "𝔢" and is
capable of printing "0" and "1". The behaviour of the machine is described
in the following table in which "R" means "the machine moves so that it scans
the square immediately on the right of the one it was scanning previously".
Similarly for "L". "E" means "the scanned symbol is erased" and "P" stands for
"prints". This table (and all succeeding tables of the same kind) is to be
understood to mean that for a configuration described in the first two columns
the operations in the third column are carried out successively, and the machine
then goes over into the m-configuration described in the last column. When the
second column is left blank, it is understood that the behaviour of the third
and fourth columns applies for any symbol and for no symbol. The machine starts
in the m-configuration b with a blank tape.

(p. 233)

...


"If (contrary to the description in § 1) we allow the letters L, R to appear
more than once in the operations column we can simplify the table considerably."

(p. 234)

-}
tm1 :: TuringMachine
tm1 = TM {
  position = 0,
  m_config = 𝔟,
  tape = take 50 $ repeat none,

  table = [
      (𝔟, None,   [    P0    ],  𝔟),
      (𝔟, Is '0', [ R, R, P1 ],  𝔟),
      (𝔟, Is '1', [ R, R, P0 ],  𝔟)
      ],

  comments = "Turing's first example machine"
  }


_None :: Symbol -> Bool
_None = (==none)

_Any :: Symbol -> Bool
_Any = (/= none)

isSymbol = _Any

{-| "As a slightly more difficult example we can construct a machine to compute
the sequence 001011011101111011111 The machine is to be capable of five
m-configurations, viz. "𝔬", "𝔮", "𝔭", "𝔣", "𝔟" and of printing "ә", "x", "0",
"1". The first three symbols on the tape will be "әә0"; the other figures follow
on alternate squares. On the intermediate squares we never print anything but
"x". These letters serve to "keep the place " for us and are erased when we have
finished with them. We also arrange that in the sequence of figures on alternate
squares there shall be no blanks."

(p. 234)
-}
tm2 :: TuringMachine
tm2 = TM {
  position = 0,
  m_config = 𝔟,
  tape = take 100 $ repeat ' ',

  table = [
      (𝔟, All, [ Pә, R, Pә, R, P0, R, R, P0, L, L ], 𝔬),
      (𝔬, Is '1',     [             R, Px, L, L, L       ], 𝔬),
      (𝔬, Is '0',     [                                  ], 𝔮),
      (𝔮, AnyOf "01", [                 R, R             ], 𝔮),
      (𝔮, None,       [                 P1, L            ], 𝔭),
      (𝔭, Is 'x',     [                 E, R             ], 𝔮),
      (𝔭, Is 'ә',     [                  R               ], 𝔣),
      (𝔭, None,       [                 L, L             ], 𝔭),
      (𝔣, Any,        [                 R, R             ], 𝔣),
      (𝔣, None,       [               P0, L, L           ], 𝔬)
      ],

  comments = "Turing's second example machine (Turing 1936)"

  }
