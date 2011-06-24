-- Currently we make the assumption that no arbitage opportunities exist.
-- That is, we assume that all exchange paths are equal.
data Exchange = Exchange {from :: [Char], to :: [Char], rate :: Double} deriving Show
type ExchangeSystem = [Exchange]

barter :: ExchangeSystem
barter = [  Exchange "banana" "bitcoin" 0.25, Exchange "bitcoin" "namecoin" 0.03,
            Exchange "apple" "banana" 0.75, Exchange "orange" "apple" 0.2
         ]

-- Only searches in one direction for now.
-- Need to add backtracking later.
appraise system thing1 thing2   | (to from_exchange) == thing2  = rate from_exchange
                                | otherwise                     = (rate from_exchange) / (appraise system (to from_exchange) thing2)
                                where from_exchange = head (dropWhile (\exch -> (from exch) /= thing1) system)
