import System.Environment
-- | POC

type State = [Float]
newtype Action = Action { perform :: (IO State, [Action]) -> (IO State, [Action]) }

mapFst f (a, b) = (f a, b)
($$) f a = f a a
tee op f g a = f a `op` g a

-- nop    = Action (\(now, suite) -> (now,                                      suite))
-- quit   = Action (\(now, suite) -> (now,                                      []   ))
-- push n = Action (\(now, suite) -> (fmap (n:) now,                            suite))
-- pop    = Action (\(now, suite) -> (fmap tail now,                            suite))
-- dup    = Action (\(now, suite) -> (fmap (\(h:t) -> h:h:t) now,               suite))
-- put    = Action (\(now, suite) -> (fmap head now >>= print >> fmap tail now, suite))
-- un op  = Action (\(now, suite) -> (fmap (\(c:t) -> op c:t) now,              suite))
-- bin op = Action (\(now, suite) -> (fmap (\(a:b:t) -> a`op`b:t) now,          suite))
-- ter op = Action (\(now, suite) -> (fmap (\(a:b:c:t) -> op a b c:t) now,      suite))

nop    = Action id
quit   = Action (flip (,) [] . fst)
push   = Action . mapFst . fmap . (:)
pop    = Action $ mapFst $ fmap tail
dup    = Action $ mapFst $ fmap (\(h:t) -> h:h:t)
put    = Action $ mapFst $ put'
un op  = Action $ mapFst $ fmap (\(c:t) -> op c:t)
bin op = Action $ mapFst $ fmap (\(a:b:t) -> a`op`b:t)
ter op = Action $ mapFst $ fmap (\(a:b:c:t) -> op a b c:t)

put' = retTail `top` putHead
     where
       retTail = flip (>>) . fmap tail
       putHead = (>>= print) . (fmap head)
       top = tee ($)

parse :: [String] -> [Action]
parse [] = []
parse (h:t) = parse' h : parse t
      where
        parse' "q" = quit
        parse' "d" = dup
        parse' "." = pop
        parse' "p" = put
        parse' "+" = bin (+)
        parse' "-" = bin (-)
        parse' "*" = bin (*)
        parse' "/" = bin (/)
        parse' "^" = bin ((. round) . (^))
        parse' "|" = ter (\a b c -> 0)
        parse' "v" = un sqrt
        parse' num = push (read num)

loop :: (IO State, [Action]) -> IO State
loop (final, []) = final
loop (state, h:t) = loop $ perform h (state, t)

run :: [Action] -> IO State -> IO State
run actions initial = loop (initial, actions)

state = pure []

main :: IO ()
main = fmap (const ()) $ flip run state . parse . concatMap words =<< getArgs
