
data Item
  = Str String
  | Num Integer
  deriving (Show)

type Stack = [Item]
-- type Registers = Map Char Item

type State = (Stack)

-- State -> String -> Num Integer

-- String -> List Char -> (String -> a) -> (a, String)
-- "123 52+"
--     ^

data Token
  = Val Item
  | Op String
  deriving (Show)


-- mkLex :: [Char] -> (String -> Token) -> String -> (Token, String)
-- mkLex l tr i = (tr . fst . (span (\c -> elem c l) i), "hey")
-- mkLex l tr i = (tr . fst . (span (\c -> elem c l) i), "hey")
-- mkLex :: [Char] -> String -> (String -> Token) -> (Token, String)
-- mkLex = span . (flip elem)

type Accepts = [Char]
type Input = String
type Output = String
type Rest = String

-- crap :: Accepts -> Input -> (Output, Rest)
-- crap = (span . (flip elem))

-- fart :: [Char] -> _
-- fart _ = mapFirst

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b) -- how is that not prelude smh, am I missing something?

swap = uncurry $ flip (,)
-- mapFst f = swap . fmap f . swap
-- crap = flip ((.) swap fmap)
-- crap = swap . (fmap (+1))
crap = swap . (flip fmap) -- (with $> somewhere..)
-- mapFst f = swap . fmap (uncurry (flip (const f)))

-- fmap :: (b -> c) -> (a, b) -> (a, c)
-- swap :: (a, b) -> (b, a)

mkLex :: Accepts -> (Output -> Token) -> Input -> (Token, Rest)
-- mkLex = (\i_to_or -> (\o_to_t -> (mapFst o_to_t) . i_to_or)) . crap
-- mkLex = (\i_to_or -> (\o_to_t -> (.) (mapFst o_to_t) i_to_or)) . crap
-- mkLex = ( flip (\o_to_t -> (.) (mapFst o_to_t)) ) . crap
-- mkLex = ( flip (\o_to_t -> ((.).mapFst) o_to_t) ) . crap
-- mkLex = flip ((.) . mapFst) . span . (flip elem)
mkLex = flip ((.) . mapFst) . span . flip elem

-- \o -> f(m(o))
-- \o -> (f.m)(o)

-- h -> (f h)

{-:
([a] -> (x, y)) -> { f -> (z, y) }
[a] -> { [a] -> ([a], [a]) }

---

(x, y) -> { f -> (z, y) }
(a -> Bool) -> { [a] -> ([a], [a]) }
[a] -> {a -> Bool}

---
(a -> Bool) -> ( [a] -> ([a], [a]) )
[a] -> (a -> Bool)

[a] -> ( [a] -> ([a], [a]) )
:-}

-- lexAdd = mkLex '+' (\_ -> "add-op")
-- lexSub = mkLex '-' (\_ -> "sub-op")

-- next :: String -> (Token, String)
-- next c::rest | c == '_'  = (Val Int 2, rest)
--              | otherwise = (Val Int 1, rest)
