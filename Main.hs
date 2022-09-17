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

-- | eg.: `f a . g a`
tee :: (c -> d -> z) -> (a -> c) -> (a -> d) -> a -> z
tee = curry . (. (. duple) . mapBoth) . (.) . uncurry

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
-- type State = (Params, Stack, RegisterMap, MacroStack)

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

-- | parses every `it ::= x`
mkMultipleLexSimple :: [(Char, Token)] -> Lexer
mkMultipleLexSimple = (uncurry fmap .) . (. duple)
                    . mapBothSndBin (flip (,) . drop 1, (. mayHead) . (=<<) . flip lookup)

-- | parses every `it ::= x <r>`
mkMultipleLexComplex :: [(Char, Char -> Token)] -> Lexer
mkMultipleLexComplex = (g `teeDot` h `teeThn`) . f
                     where
                       f = ((. mayHead) . (=<<) . flip lookup) -- get fr :: (Char -> Token)
                       g = flip fmap . mayHead . drop 1 -- get r :: Char and pass it through fr
                       h = (.) . flip (,) . drop 2 -- build the result (Token, Rest)
                       teeDot = tee (.)
                       teeThn = tee (=<<)

-- | parses every `it ::= y x <r>`
mkMultipleLexComplex2 :: [(Char, [(Char, Char -> Token)])] -> Lexer
mkMultipleLexComplex2 = (flip mksublex `teeThn`) . f
                      where
                        f = ((. mayHead) . (=<<) . flip lookup) -- get t' :: [(Char, Char -> Token)]
                        mksublex = (. drop 1) . mkMultipleLexComplex
                        teeThn = tee (=<<)

-- | mkLexLong pb pc (f c)
--   parses `it ::= ?pb {?pc}`
--   where pb and pc are predicate begin and continue
-- YYY: write once...
mkSingleLexLong :: (Char -> Bool) -> (Char -> Bool) -> (Output -> Token) -> Lexer
mkSingleLexLong = (. (. f) . g) . (.) . (. fmap) . (|.) . mayIfHead
                where
                  f = (uncurry . mapBothFstBin)
                    . (flip (,)) id . (. (:) . head) . (.)
                  g = (|.) . (. duple) . mapSnd . (. drop 1) . span

lexAllSimple = mkMultipleLexSimple
  [ ('p', (Op "pln"))
  , ('n', (Op "nln"))
  , ('P', (Op "pnt"))
  , ('f', (Op "dmp"))
  , ('+', (Op "add"))
  , ('-', (Op "sub"))
  , ('*', (Op "mul"))
  , ('/', (Op "div"))
  , ('%', (Op "rem"))
  , ('~', (Op "quo"))
  , ('^', (Op "exp"))
  , ('|', (Op "mex"))
  , ('v', (Op "srt"))
  , ('c', (Op "clr"))
  , ('d', (Op "dup"))
  , ('r', (Op "swp"))
  , ('R', (Op "rot"))
  , ('i', (Op "sir"))
  , ('o', (Op "sor"))
  , ('k', (Op "spr"))
  , ('I', (Op "gir"))
  , ('O', (Op "gor"))
  , ('K', (Op "gpr"))
  , ('a', (Op "bla"))
  , ('x', (Op "exc"))
  , ('?', (Op "rdx"))
  , ('q', (Op "qui"))
  , ('Q', (Op "mqu"))
  , ('Z', (Op "ndd"))
  , ('X', (Op "nfd"))
  , ('z', (Op "dpt"))
  ]

lexAllComplex = mkMultipleLexComplex
  [ ('s', (Op . (flip (:) "#ser")))
  , ('l', (Op . (flip (:) "#ger")))
  , ('S', (Op . (flip (:) "#pur")))
  , ('L', (Op . (flip (:) "#por")))
  , ('>', (Op . (flip (:) "#gt")))
  , ('<', (Op . (flip (:) "#lt")))
  , ('=', (Op . (flip (:) "#eq")))
  -- , (':', (Op . (flip (:) "#set")))
  -- , (';', (Op . (flip (:) "#get")))
  ]

lexAllComplex2 = mkMultipleLexComplex2
  [ ('!', [ ('>', (Op . (flip (:) "#le")))
          , ('<', (Op . (flip (:) "#ge")))
          , ('=', (Op . (flip (:) "#ne")))
          ])
  ]

lexNumber = (mkSingleLexLong $$) (flip elem $ "0123456789") (Val . Num . read)
lexString = mkSingleLexLong (=='[') (/=']') (Val . Str . tail) -- FIXME: capture closing ']'
lexSkip = (mkSingleLexLong $$) (flip elem $ "\t\n\r ]") (const (Op "nop"))
lexCommand = mkSingleLexLong (=='!') (/='\n') (const (Op "sh . drop first"))
lexComment = mkSingleLexLong (=='#') (/='\n') (const (Op "nop"))

lexers =
  [ lexNumber
  , lexSkip
  , lexString
  , lexComment
  , lexAllComplex2
  , lexCommand
  , lexAllComplex
  , lexAllSimple
  ]

next :: Input -> (Token, Rest)
next = sure -- ok because of `dropWhile`
     . maybe (error "unexpected token smth here") id -- YYY: again on craching is not the best
     . mayHead
     . dropWhile (== Nothing)
     . (flip pam) lexers
     where pam = map . (|$)

does :: Token -> Token --Action
does = id --const id

parse :: Input -> [Token] --[Action]
parse = map (does . fst . fst) -- YYY: filter nops somewhere
      . takeWhile (not . null . snd . snd)
      . zip1Off
      . iterate (next . snd)
      . (,) undefined
      where zip1Off = uncurry (zip . drop 1) . duple

--- entry point
--main and such
main :: IO ()
main = interact $ show . parse
