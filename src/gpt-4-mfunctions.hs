type Symbol = Char
type MConfig = String
type MFunction = (MConfig, MConfig, Symbol) -> MConfig

data Action = L | R deriving (Eq, Show)
type State = (MConfig, [Symbol], [Symbol])

move :: Action -> [Symbol] -> [Symbol] -> ([Symbol], [Symbol])
move L ls (r:rs) = (r:ls, rs)
move R (l:ls) rs = (ls, l:rs)

f, f1, f2 :: State -> State
f (c, b, a) = let (ls, rs) = move L b a in f1 (c, ls, rs)

-- Correction:
-- It had written 'None'
none = ' '
-- 

-- Correction

f1 (c, b, a)
  | current == target = (c, ls, rs)
  | current == none = f2 (c, ls, rs)
  | otherwise = f1 (c, ls, rs)
  where
    (ls, rs) = move R b a
    current = head b
    target = head rs

f2 (c, b, a)
  | current == target = (c, ls, rs)
  | current == none = (c, ls, rs)
  | otherwise = f1 (c, ls, rs)
  where
    (ls, rs) = move R b a
    current = head b
    target = head rs

skeletonTable :: (State -> State) -> Symbol -> [Symbol] -> MConfig -> State
skeletonTable mf target tape mc = mf (mc, [], target:tape)


