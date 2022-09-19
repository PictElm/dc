import System.Environment -- for `getArgs` - this is so frustrating

--- utils
(|.) = flip (.)
(|$) = flip ($)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst = uncurry . (.) (,)

mapSnd :: (b -> d) -> (a, b) -> (a, d)
mapSnd = fmap

duple :: a -> (a, a)
-- NOTE: with duple, seems most of the time is lost on `replicate`
--       so yes, it can be point free, but also twice the cost
-- duple = (mapSnd head . mapFst head)
--       . (splitAt 1 . replicate 2)
duple a = (a, a)
-- duple = ($$) (,)

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
-- YYY: probably can be better

sure :: Maybe a -> a
sure = maybe undefined id

--- calculator
data Item
  = Num Integer
  | Str String
  deriving (Show, Eq)

type Stack = [Item]
-- type Registers = Map Char (Stack, [Item])

type State = (Stack)
-- type MacroStack = [Action]
-- type State = (Params, Stack, RegisterMap, MacroStack)

type Action = String --State -> State

exec :: [Action] -> Action
exec = foldl1 (<>) --(|.)

--- lexers/scanners/parsers
type Token = Maybe Action

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
-- YYY: write once... (would like to rewrite)
mkSingleLexLong :: (Char -> Bool) -> (Char -> Bool) -> (Output -> Token) -> Lexer
mkSingleLexLong = (. (. f) . g) . (.) . (. fmap) . (|.) . mayIfHead
                where
                  f = (uncurry . mapBothFstBin)
                    . (flip (,)) id . (. (:) . head) . (.)
                  g = (|.) . (. duple) . mapSnd . (. drop 1) . span

lexAllSimple = mkMultipleLexSimple
  [ ('p', (Just "pln"))
  , ('n', (Just "nln"))
  , ('P', (Just "pnt"))
  , ('f', (Just "dmp"))
  , ('+', (Just "add"))
  , ('-', (Just "sub"))
  , ('*', (Just "mul"))
  , ('/', (Just "div"))
  , ('%', (Just "rem"))
  , ('~', (Just "quo"))
  , ('^', (Just "exp"))
  , ('|', (Just "mex"))
  , ('v', (Just "srt"))
  , ('c', (Just "clr"))
  , ('d', (Just "dup"))
  , ('r', (Just "swp"))
  , ('R', (Just "rot"))
  , ('i', (Just "sir"))
  , ('o', (Just "sor"))
  , ('k', (Just "spr"))
  , ('I', (Just "gir"))
  , ('O', (Just "gor"))
  , ('K', (Just "gpr"))
  , ('a', (Just "bla"))
  , ('x', (Just "exc"))
  , ('?', (Just "rdx"))
  , ('q', (Just "qui"))
  , ('Q', (Just "mqu"))
  , ('Z', (Just "ndd"))
  , ('X', (Just "nfd"))
  , ('z', (Just "dpt"))
  ]

lexAllComplex = mkMultipleLexComplex
  [ ('s', (Just . (flip (:) "#ser")))
  , ('l', (Just . (flip (:) "#ger")))
  , ('S', (Just . (flip (:) "#pur")))
  , ('L', (Just . (flip (:) "#por")))
  , ('>', (Just . (flip (:) "#gt")))
  , ('<', (Just . (flip (:) "#lt")))
  , ('=', (Just . (flip (:) "#eq")))
  -- , (':', (Just . (flip (:) "#set")))
  -- , (';', (Just . (flip (:) "#get")))
  ]

lexAllComplex2 = mkMultipleLexComplex2
  [ ('!', [ ('>', (Just . (flip (:) "#le")))
          , ('<', (Just . (flip (:) "#ge")))
          , ('=', (Just . (flip (:) "#ne")))
          ])
  ]

lexNumber = (mkSingleLexLong $$) (flip elem $ "0123456789") (Just . ("pushNum " ++))
lexString = mkSingleLexLong (=='[') (/=']') (Just . ("pushStr " ++) . drop 1) -- FIXME: capture closing ']'
lexSkip = (mkSingleLexLong $$) (flip elem $ "\t\n\r ]") (const Nothing)
lexCommand = mkSingleLexLong (=='!') (/='\n') (const (Just "sh . drop 1"))
lexComment = mkSingleLexLong (=='#') (/='\n') (const Nothing)

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
     -- . maybe (Just (Nothing, xyz)) id -- this with xyz (drop 1) of the Input
     . mayHead
     . dropWhile (== Nothing)
     . (flip pam) lexers
     where pam = map . (|$)

parse :: Input -> [Action]
parse = map sure
      . filter (/= Nothing)
      . map (fst . fst)
      . takeWhile notEOF
      . zip1Off
      . iterate (next . snd)
      . (,) undefined
      where
        zip1Off = uncurry (zip . drop 1) . duple
        notEOF = not . null . snd . snd

--- entry point
type Arg = String

resolveArgs :: [Arg] -> [IO Input] -- DOC: unlike dc (on execution order)
resolveArgs = uncurry recurse . process
            where
              recurse :: IO Input -> [Arg] -> [IO Input]
              recurse = (. tee (maybe []) (const . resolveArgs) mayHead) . (:)
              process :: [Arg] -> (IO Input, [Arg])
              process = ($$) (maybe
                        (mapFst (readFile . head) . splitAt 1) id -- default option
                        . flip lookup options . (take 2 . head) -- search for existing option
                      ) -- ^ (both) head ok because condition in `recurse` and default in `getInput`
              options :: [([Char], [Arg] -> (IO Input, [Arg]))]
              options =
                [ ("-V", const (pure "[loosely based on dc (GNU db 1.07.1) 1.4.1]pq", []))
                , ("-h", const (pure "[most options and commands are compatible with dc(1);\nsee its man page for more informations]pq", []))
                , ("-e", \args -> (pure "[expression]", drop 1 args)) -- uses next arg if length head is 2
                , ("-f", \args -> (pure "[scriptfile]", drop 1 args)) -- uses next arg if length head is 2
                , ("--", const (pure "[long options are not implemented (yet(?))]pq", []))
                , ("-", (,) getContents . (drop 1))
                ]

getInput :: IO Input
getInput = fmap unwords (withDefaultArgs ["-"] >>= sequence . resolveArgs)
         where withDefaultArgs = (<$> getArgs)
                               . (. mapBoth (const, mayHead) . duple)
                               . uncurry . maybe

main :: IO ()
main = (exec . parse) <$> getInput >>= print
