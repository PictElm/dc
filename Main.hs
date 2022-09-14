--- utils
mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst = uncurry . (.) (,)

mapSnd :: (b -> d) -> (a, b) -> (a, d)
mapSnd = fmap

mayHead :: [a] -> Maybe a
mayHead = foldr (const . Just) Nothing

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

mayProcess :: (Output -> Token) -> (String, Rest) -> Maybe (Token, Rest)
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

next :: Input -> (Token, Rest)
next s = maybe (error "unexpected token smth here") id
         (head $ dropWhile (== Nothing) -- XXX: will ignore the last token
           [ lexNum s
           , lexAdd s
           , lexSub s
           ]) -- TODO: inside out
-- head $ dropWhile (== Nothing) (map ($) lexers s)

does :: Token -> Action
does = const id

parse :: Input -> [Action]
parse = map (does . fst)
      . takeWhile (not . null . snd)
      . iterate (next . snd)
      . (,) (Op "init") -- YYY: remove or make use of

--- entry point
--main and such
