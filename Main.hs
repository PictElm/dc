
data Item
  = Str String
  | Num Integer
  deriving (Show)

type Stack = [Item]
-- type Registers = Map Char Stack

type State = (Stack)

-- State -> String -> Num Integer

-- String -> List Char -> (String -> a) -> (a, String)
-- "123 52+"
--     ^

data Token
  = Val Item
  | Op String
  deriving (Show)


type Accepts = [Char]
type Input = String
type Output = String
type Rest = String

map2 :: (a -> b -> (c, d)) -> (a, b) -> (c, d)
map2 = uncurry

applyFst :: (a -> c) -> a -> d -> (c, d)
applyFst = (.) (,)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst = map2 . applyFst

mkLex :: Accepts -> (Output -> Token) -> Input -> (Token, Rest)
mkLex = flip ((.) . mapFst) . span . flip elem

-- lexAdd = mkLex '+' (\_ -> "add-op")
-- lexSub = mkLex '-' (\_ -> "sub-op")

-- next :: String -> (Token, String)
-- next c::rest | c == '_'  = (Val Int 2, rest)
--              | otherwise = (Val Int 1, rest)
