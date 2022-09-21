import System.Environment -- for `getArgs`
import System.Process -- for `system`

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

type Program = [Action] -- newtype Action below

type Stack = [Item] -- (why not with nest tuple?)
data Params = Params
  { inBase :: Int
  , outBase :: Int
  , precision :: Int
  }
type Registers = [(Char, Stack)] -- (Char, (Stack, [Item]))
type MacroStack = [Program]

data State = State
  { stack :: Stack
  , params :: Params
  , registers :: Registers
  , macroStack :: MacroStack
  }
clean = State [] (Params 10 10 0) [] []

newtype Action = Action
  { perform :: (IO State, Program) -> (IO State, Program) }
opUnknown = Action . mapFst . (>>) . print . (:": operation unimplemented") -- TODO: use stderr

loop :: (IO State, Program) -> IO State
loop = ($$) (tfa (mayHead . snd) -- get head into Maybe
     . (. fst) . flip maybe -- if Nothing, fst (the IO State)
     . (loop .) . flip perform . mapSnd tail) -- else perform action and loop
     where tfa = tee (|$)

run :: Program -> IO State -> IO State
run = flip $ curry loop

--- lexers/scanners/parsers
type Token = Maybe Action

type Lexer = Input -> Maybe (Token, Rest)

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

nop = Action id -- TODO: remove, actual nop is implemented with Maybe

-- TODO: instances and such or something idk
addCrap :: Item -> Item -> Item
addCrap (Num a) (Num b) = Num (a+b)
addCrap (Str a) (Str b) = Str (a++b)

lexAllSimple = mkMultipleLexSimple
  [ ('p', Just$Action (\(s, l) -> (s >>= print.(maybe(error"print: stack empty")id.mayHead).stack >> s, l)))
  , ('n', Just nop)
  , ('P', Just nop)
  , ('f', Just nop)
  , ('+', Just$Action (\(s, l) -> ((\s' -> s'{stack=((\(a:b:t) -> addCrap a b:t).stack)s'}) <$> s, l)))
  , ('-', Just nop)
  , ('*', Just nop)
  , ('/', Just nop)
  , ('%', Just nop)
  , ('~', Just nop)
  , ('^', Just nop)
  , ('|', Just nop)
  , ('v', Just nop)
  , ('c', Just nop)
  , ('d', Just nop)
  , ('r', Just nop)
  , ('R', Just nop)
  , ('i', Just nop)
  , ('o', Just nop)
  , ('k', Just nop)
  , ('I', Just nop)
  , ('O', Just nop)
  , ('K', Just nop)
  , ('a', Just nop)
  , ('x', Just nop)
  , ('?', Just nop)
  , ('q', Just nop)
  , ('Q', Just nop)
  , ('Z', Just nop)
  , ('X', Just nop)
  , ('z', Just nop)
  ]

lexAllComplex = mkMultipleLexComplex
  [ ('s', const$Just nop)
  , ('l', const$Just nop)
  , ('S', const$Just nop)
  , ('L', const$Just nop)
  , ('>', const$Just nop)
  , ('<', const$Just nop)
  , ('=', const$Just nop)
  -- , (':', (Just . (flip (:) "#set")))
  -- , (';', (Just . (flip (:) "#get")))
  ]

lexAllComplex2 = mkMultipleLexComplex2
  [ ('!', [ ('>', const$Just nop)
          , ('<', const$Just nop)
          , ('=', const$Just nop)
          ])
  ]

lexNumber = (mkSingleLexLong $$) (flip elem $ "0123456789") (\c -> Just$Action (\(s, l) -> ((\s' -> s'{stack=(((Num .read)c:).stack)s'}) <$> s, l)))
lexString = mkSingleLexLong (== '[') (/= ']') (const$Just nop) -- FIXME: capture closing ']'
lexSkip = (mkSingleLexLong $$) (flip elem $ "\t\n\r ]") (const$Nothing)
lexCommand = mkSingleLexLong (== '!') (/= '\n') (const$Just nop)
lexComment = mkSingleLexLong (== '#') (/= '\n') (const$Nothing)

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
next = ((sure .) . orUnkOp) `ta` findLex -- `sure`: ok because of `orUnkOp`
     where -- YYY: probably one too many 'sure/maybe' but anyway
       orUnkOp = flip maybe id
               . Just . mapBoth (Just . opUnknown . head, drop 1)
               . duple
       findLex = mayHead
               . dropWhile (maybe True (const False))
               . (flip pam) lexers
       pam = map . (|$)
       ta = tee ($)

parse :: Input -> Program
parse = map sure -- ok because of `filter`
      . filter (maybe False (const True)) -- filters nops
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

procShortOption :: (Arg -> r) -> [Arg] -> (r, [Arg])
procShortOption = ($$) . flip (maybe nextOne (const thisOne) . mayHead . drop 2 . head)
                where -- v (each) first head ok because called from `resolveArgs`
                  thisOne = (. duple) . mapBothFstBin ((. drop 2 . head), drop 1)
                  nextOne = (. duple) . mapBothFstBin ((. maybe (error "option requires an argument") id . mayHead . tail), drop 2)

procLongOption :: (Arg -> r) -> [Arg] -> (r, [Arg])
procLongOption = (. splitAt 1) . mapFst . (. drop 1 . dropWhile (/= '=') . head)

version = "[loosely based on dc (GNU db 1.07.1) 1.4.1]pq"
help = "[most options and commands are compatible with dc(1);\nsee its man page for more information]pq"

resolveArgs :: [Arg] -> [IO Input] -- DOC: unlike dc (on execution order)
resolveArgs = uncurry recurse . process
            where
              recurse = (. tee (maybe []) (const . resolveArgs) mayHead) . (:)
              process = ($$) (maybe
                        (mapFst (readFile . head) . splitAt 1) id -- default option
                        . flip lookup options . (take 2 . head) -- search for existing option
                      ) -- ^ (both) head ok because condition in `recurse` and default in `getInput`
              process' = ($$) (maybe
                         (error "unrecognized option") id -- fail on unknown
                         . flip lookup options' . (takeWhile (/= '=') . head) -- search for existing long option
                       ) -- ^ head still ok for same reason
              options =
                [ ("-V", const (pure version, []))
                , ("-h", const (pure help, []))
                , ("-e", procShortOption pure)
                , ("-f", procShortOption readFile)
                , ("--", process')
                , ("-", (,) getContents . (drop 1)) -- unwords . words . --?
                -- , ("-", (,) (unlines <$> iterate (const getLine) (pure "")) . (drop 1))
                ]
              options' =
                [ ("--version", const (pure version, []))
                , ("--help", const (pure help, []))
                , ("--expression", procLongOption pure)
                , ("--file", procLongOption readFile)
                ]

getInput :: IO Input
getInput = fmap unwords (withDefaultArgs ["-"] >>= sequence . resolveArgs)
         where withDefaultArgs = (<$> getArgs)
                               . (. mapBoth (const, mayHead) . duple)
                               . uncurry . maybe

main :: IO ()
main = fmap (const ()) $ flip run (pure clean) . parse =<< getInput
