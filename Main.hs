--- utils
mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst = uncurry . (.) (,)

mapSnd :: (b -> d) -> (a, b) -> (a, d)
mapSnd = fmap

mayHead :: [a] -> Maybe a
mayHead = foldr (const . Just) Nothing

duple :: a -> (a, a)
duple = (mapSnd head . mapFst head)
      . (splitAt 1 . replicate 2)

--- calculator
data Item
  = Str String
  | Num Integer
  deriving (Show, Eq)

type Stack = [Item]
-- type Registers = Map Char Stack

type State = (Stack)

type Action = State -> State

--- parser
data Token
  = Val Item
  | Op String --Action
  deriving (Show, Eq)

type Lexer = Input -> Maybe (Token, Rest)

type Accepts = [Char]
type Input = String
type Output = String
type Rest = String

mayProcess :: (Output -> Token) -> (Output, Rest) -> Maybe (Token, Rest)
mayProcess = flip $ flip (fmap . mapFst)
           -- YYY: could do a `mayIf :: p -> a -> Maybe a`
           . mayHead
           . takeWhile (not . null . fst)
           . replicate 1

mkLex :: Accepts -> (Output -> Token) -> Lexer
mkLex = flip ((.) . mayProcess) . span . flip elem

lexNum = mkLex "1234567890" (Val . Num . read)
lexAdd = mkLex ['+'] (const (Op "add"))
lexSub = mkLex ['-'] (const (Op "sub"))
lexers =
  [ lexNum
  , lexAdd
  , lexSub
  ]

next :: Input -> (Token, Rest)
next = maybe undefined id -- unreachable (because of `dropWhile`)
     . maybe (error "unexpected token smth here") id
     . mayHead
     . dropWhile (== Nothing)
     . (flip pam) lexers
     where pam = map . flip ($)

does :: Token -> Action
does = const id

parse :: Input -> [Action]
parse = map (does . fst . snd)
      . takeWhile (not . null . snd . fst)
      . zip1Off
      . iterate (next . snd)
      . (,) undefined
      where zip1Off = uncurry (flip zip . drop 1) . duple

exec :: [Action] -> Action
exec = foldl1 (flip (.))

--- entry point
--main and such
