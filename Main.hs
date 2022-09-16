--- utils
(|.) = flip (.)
(|$) = flip ($)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst = uncurry . (.) (,)

mapSnd :: (b -> d) -> (a, b) -> (a, d)
mapSnd = fmap

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

mapBothFstBin :: (t -> a -> c, b -> d) -> t -> (a, b) -> (c, d)
mapBothFstBin = (.) mapBoth . flip (mapFst . (|$))

mapBothSndBin :: (a -> c, t -> b -> d) -> t -> (a, b) -> (c, d)
mapBothSndBin = (.) mapBoth . flip (mapSnd . (|$))

mayHead :: [a] -> Maybe a
mayHead = foldr (const . Just) Nothing

mayIfHead :: (Char -> Bool) -> Input -> Maybe Input
mayIfHead = (. duple) . (.) (uncurry fmap)
            . mapBothSndBin (const, (mayHead .) . takeWhile)

sure :: Maybe a -> a
sure = maybe undefined id

--- calculator
data Item
  = Str String
  | Num Integer --String
  deriving (Show, Eq)

type Stack = [Item]
-- type Registers = Map Char (Stack, [Item])

type State = (Stack)
-- type MacroStack = [Action]
-- type State = (Stack, RegisterMap, MacroStack)

type Action = State -> State

exec :: [Action] -> Action
exec = foldl1 (|.)

--- lexers/scanners/parsers
data Token -- YYY: will probably Token = Action soon
  = Val Item
  | Op String --Action
  deriving (Show, Eq)

type Lexer = Input -> Maybe (Token, Rest)
type SureLexer = Input -> (Token, Rest)

type Accepts = [Char]
type Input = String
type Output = [Char]
type Rest = String

-- | mkLexSimple x
--   parses `it ::= x`
mkLexSimple :: Char -> Token -> Lexer
mkLexSimple = (. const) . flip mkLexLong (const False) . (==)

mkSureLexComplex :: (Char -> Token) -> SureLexer
mkSureLexComplex = (. duple)
                 . mapBothFstBin ((. head . drop 1), drop 2)
-- YYY: at head: "error: expected register next"
--      but this is not a solution (to crash); it
--      should just have been Nothing on the level
--      of the may lexer

-- | mkLexComplex x (f r)
--   parses `it ::= x <r>`
--   and r will be any character
mkLexComplex :: Char -> (Char -> Token) -> Lexer
mkLexComplex = (. fmap . mkSureLexComplex) . (|.) . mayIfHead . (==)

-- | mkLexComplex2 y x (f r)
--   parses `it ::= y x <r>`
--   and r will be any character
mkLexComplex2 :: Char -> Char -> (Char -> Token) -> Lexer
mkLexComplex2 = (. mkLexComplex) . (.) . (. (=<<)) . (|.) . (.) (drop 1 <$>) . mayIfHead . (==)

mkSureLexLong :: (Char -> Bool) -> (Output -> Token) -> SureLexer
mkSureLexLong = (. f) . g
              where
                f = (uncurry . mapBothFstBin)
                  . (flip (,)) id . (. (:) . head) . (.)
                g = (|.) . (. duple) . mapSnd . (. drop 1) . span

-- | mkLexLong pb pc (f c)
--   parses `it ::= ?pb {?pc}`
--   where pb and pc are predicate begin and continue
mkLexLong :: (Char -> Bool) -> (Char -> Bool) -> (Output -> Token) -> Lexer
mkLexLong = (. mkSureLexLong) . (.) . (. fmap) . (|.) . mayIfHead

lexNum = (mkLexLong $$) (flip elem $ "1-9A-Z_") (Val . Num . read)
lexStr = mkLexLong (=='[') (/=']') (Val . Str . tail)
lexSkip = (mkLexLong $$) (flip elem $ "\t\n\r ]") (const (Op "nop"))
-- printing commands
lexPln = mkLexSimple 'p' (Op "pln")
lexNln = mkLexSimple 'n' (Op "nln")
lexPnt = mkLexSimple 'P' (Op "pnt")
lexDmp = mkLexSimple 'f' (Op "dmp")
-- arithmetic
lexAdd = mkLexSimple '+' (Op "add")
lexSub = mkLexSimple '-' (Op "sub")
lexMul = mkLexSimple '*' (Op "mul")
lexDiv = mkLexSimple '/' (Op "div")
lexRem = mkLexSimple '%' (Op "rem")
lexQuo = mkLexSimple '~' (Op "quo")
lexExp = mkLexSimple '^' (Op "exp")
lexMex = mkLexSimple '|' (Op "mex")
lexSrt = mkLexSimple 'v' (Op "srt")
-- stack control
lexClr = mkLexSimple 'c' (Op "clr")
lexDup = mkLexSimple 'd' (Op "dup")
lexSwp = mkLexSimple 'r' (Op "swp")
lexRot = mkLexSimple 'R' (Op "rot")
-- registers
lexSer = mkLexComplex 's' (Op . ((flip (:)) "#ser"))
lexGer = mkLexComplex 'l' (Op . ((flip (:)) "#ger"))
lexPur = mkLexComplex 'S' (Op . ((flip (:)) "#pur"))
lexPor = mkLexComplex 'L' (Op . ((flip (:)) "#por"))
-- parameters
lexSir = mkLexSimple 'i' (Op "sir")
lexSor = mkLexSimple 'o' (Op "sor")
lexSpr = mkLexSimple 'k' (Op "spr")
lexGir = mkLexSimple 'I' (Op "gir")
lexGor = mkLexSimple 'O' (Op "gor")
lexGpr = mkLexSimple 'K' (Op "gpr")
-- strings (& macros)
lexBla = mkLexSimple 'a' (Op "bla")
lexExc = mkLexSimple 'x' (Op "exc")
lexGt = mkLexComplex '>' (Op . ((flip (:)) "#gt"))
lexNGt = mkLexComplex2 '!' '>' (Op . ((flip (:)) "#le"))
lexLt = mkLexComplex '<' (Op . ((flip (:)) "#lt"))
lexNLt = mkLexComplex2 '!' '<' (Op . ((flip (:)) "#ge"))
lexEq = mkLexComplex '=' (Op . ((flip (:)) "#eq"))
lexNEq = mkLexComplex2 '!' '=' (Op . ((flip (:)) "#ne"))
lexRdx = mkLexSimple '?' (Op "rdx")
lexQui = mkLexSimple 'q' (Op "qui")
lexMqu = mkLexSimple 'Q' (Op "mqu")
-- status inquiry
lexNdd = mkLexSimple 'Z' (Op "ndd")
lexNfd = mkLexSimple 'X' (Op "nfd")
lexDpt = mkLexSimple 'z' (Op "dpt")
-- miscellianeous
lexCmd = mkLexLong (=='!') (/='\n') (const (Op "sh . drop first"))
lexCmt = mkLexLong (=='#') (/='\n') (const (Op "nop"))
--lexEq = mkLexComplex ':' (Op . ((flip (:)) "#set"))
--lexEq = mkLexComplex ';' (Op . ((flip (:)) "#get"))

lexers =
  [ lexNum
  , lexStr
  , lexSkip
  , lexPln
  , lexNln
  , lexPnt
  , lexDmp
  , lexAdd
  , lexSub
  , lexMul
  , lexDiv
  , lexRem
  , lexQuo
  , lexExp
  , lexMex
  , lexSrt
  , lexClr
  , lexDup
  , lexSwp
  , lexRot
  , lexSer
  , lexGer
  , lexPur
  , lexPor
  , lexSir
  , lexSor
  , lexSpr
  , lexGir
  , lexGor
  , lexGpr
  , lexBla
  , lexExc
  , lexGt
  , lexNGt
  , lexLt
  , lexNLt
  , lexEq
  , lexNEq
  , lexRdx
  , lexQui
  , lexMqu
  , lexNdd
  , lexNfd
  , lexDpt
  , lexCmd
  , lexCmt
  ]

next :: Input -> (Token, Rest)
next = sure -- ok because of `dropWhile`
     . maybe (error "unexpected token smth here") id
     . mayHead
     . dropWhile (== Nothing)
     . (flip pam) lexers
     where pam = map . (|$)

does :: Token -> Token --Action
does = id --const id

parse :: Input -> [Token] --[Action]
parse = map (does . fst . fst)
      . takeWhile (not . null . snd . snd)
      . zip1Off
      . iterate (next . snd)
      . (,) undefined
      where zip1Off = uncurry (zip . drop 1) . duple

--- entry point
--main and such
