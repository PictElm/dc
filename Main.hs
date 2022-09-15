--- utils
(|.) = flip (.)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst = uncurry . (.) (,)

mapSnd :: (b -> d) -> (a, b) -> (a, d)
mapSnd = fmap

mayHead :: [a] -> Maybe a
mayHead = foldr (const . Just) Nothing

duple :: a -> (a, a)
duple = (mapSnd head . mapFst head)
      . (splitAt 1 . replicate 2)

($$) :: (a -> a -> b) -> a -> b
($$) = flip (flip uncurry . duple)

mapBoth :: (a -> c, b -> d) -> (a, b) -> (c, d)
mapBoth = ($$) (flip . (flip x .) . y)
        where
          x = mapFst . fst
          y = mapSnd . snd

--- calculator
data Item
  = Str String
  | Num Integer --String
  deriving (Show, Eq)

type Stack = [Item]
-- type Registers = Map Char (Stack, [Item])

type State = (Stack)

type Action = State -> State

--- parser
data Token -- YYY: will probably Token = Action soon
  = Val Item
  | Op String --Action
  deriving (Show, Eq)

type Lexer = Input -> Maybe (Token, Rest)

{-
script ::= {action}
action ::= op | val

val ::= num | str
num ::= /[0-9A-Z]+/
str ::= /\[.*\]/

op ::=
  | /[pnPf]/       -- printing commands
  | /[+\-*/%~^|v]/ -- arithmetic
  | /[cdrR]/       -- stack control
  | /[slSL]\w/     -- registers
  | /[iokIOK]/     -- parameters
  | /[ax]/         -- strings (& macros)
  | /!?[<=>]\w/
  | /[\?qQ]/
  | /[ZXz]/        -- statud inquiry
  | /[!#].*\n/     -- miscellianeous
  | /[:;]\w/       -- no (or not yet)


simpleOp   ::=   x
complexOp  ::=   x r
complexOp2 ::= y x r
longOp     ::= b c*
delimOp is longOp where c is .\e


longOp ::= pb cb
-}

type Accepts = [Char]
type Input = String
type Output = String
type Rest = String

lexnt :: Lexer
lexnt = const (Just (Op "nop", ""))

mkLexSimple :: Char -> Token -> Lexer
mkLexSimple x t = mkLexLong (==x) (const False) (const t)

mkLexComplex :: Char -> (Char -> Token) -> Lexer
-- mkLexComplex x fr = \i ->
--                       if x == head i
--                         then Just (fr (head (drop 1 i)), drop 2 i)
--                         else Nothing
mkLexComplex x fr = fmap (mkSureLexComplex fr)
                  . mayIfStarts x
mkSureLexComplex :: (Char -> Token) -> Input -> (Token, Rest)
mkSureLexComplex fr = mapBoth t . duple
                    where t = ( (. head . drop 1) fr
                              , drop 2
                              ) -- TODO: mapFst
mayIfStarts :: Char -> Input -> Maybe Input
mayIfStarts x = uncurry fmap . mapBoth t . duple
              where t = ( const
                        , ( (mayHead .) . takeWhile . (==) ) x
                        ) -- TODO: mapSnd

mkLexComplex2 :: Char -> Char -> (Char -> Token) -> Lexer
mkLexComplex2 y x fr = \i ->
                         if y == head i
                           then (mkLexComplex x fr) (drop 1 i)
                           else Nothing

mkLexLong :: (Char -> Bool) -> (Char -> Bool) -> ([Char] -> Token) -> Lexer
mkLexLong pb pc fc = \i ->
                       if pb (head i) -- fmap (\c -> ..) . mayHead
                         then
                           let
                             c = head i
                             (o, r) = span pc (drop 1 i)
                           in Just (fc (c:o), r)
                         else Nothing
-- mkLexLong ~= ..?


lexAdd = mkLexSimple '+' (Op "add")
lexSub = mkLexSimple '-' (Op "sub")

lexLt = mkLexComplex '<' (Op . ((flip (:)) ": ifLt (exec . getR)"))
lexNLt = mkLexComplex2 '!' '<' (Op . ((flip (:)) ": ifGt (exec . getR)"))

lexNum = (($$) mkLexLong) (flip elem $ "1-9A-Z_") (Val . Num . read)
lexStr = mkLexLong (=='[') (/=']') (Val . Str . tail)
-- YYY: hence needs to discard the ']' (see `lexSkip`)

lexCmd = mkLexLong (=='!') (/='\n') (const (Op "sh . drop first"))
lexCmt = mkLexLong (=='#') (/='\n') (const (Op "nop"))

-- lexSkip = (($$) mkLexLong) (isBlank or ']') (const (Op "nop"))

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

does :: Token -> Token --Action
does = id --const id

parse :: Input -> [Token] --[Action]
parse = map (does . fst . fst)
      . takeWhile (not . null . snd . snd)
      . zip1Off
      . iterate (next . snd)
      . (,) undefined
      where zip1Off = uncurry (zip . drop 1) . duple

exec :: [Action] -> Action
exec = foldl1 (|.)

--- entry point
--main and such
