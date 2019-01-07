import Control.Applicative
import Data.Char (toUpper)
import Data.List (intercalate, (\\), tails, find)
import System.Environment (getArgs)
import System.Directory (getHomeDirectory, doesFileExist)

main = do
    args <- getArgs
    if anyEq ["-h", "--help"] args
    then do
        help <- readFile "README.md"
        putStr help
    else do
        home <- getHomeDirectory
        let expPath = [ "exp.txt", home ++ "exp.txt"]
        exp  <- expLocate $ args ++ expPath
        heading "stats"
        putStr
            $  statShow "Drugs" drugCount exp
            ++ statShow "Combos" comboCount exp
            ++ statShow "Total" totalCount exp
        heading "duplicates"
        putStr
            $ pretty
            $ onlyDupes
            $ dupes
            $ wordList exp

-- General --------------------

type Drug = String

type Drugs = [Drug]

type Exp = [Drugs]

data Dupe = Dupe { exists :: Bool
                 , first  :: Drug
                 , second :: Drug
                 } deriving (Show)

type Dupes = [Dupe]

anyEq :: Eq a => [a] -> [a] -> Bool
anyEq x y = any id $ (==) <$> x <*> y

heading :: String -> IO ()
heading s = putStrLn $ "\n" ++ x ++ "\n"
    where x = take 40 $ (map toUpper s) ++ " " ++ repeat '-'

expLocate :: [FilePath] -> IO String
expLocate []     = error "No experiences file found.\
    \ See help: [-h] or [--help]"
expLocate (x:xs) = do
    exists <- doesFileExist x
    if exists then readFile x else expLocate xs

-- Stats --------------------

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = (if x `elem` xs then id else (x:)) $ unique xs

drugCount :: String -> Int
drugCount = length . unique . words

totalCount :: String -> Int
totalCount = length . lines

comboCount :: String -> Int
comboCount s = totalCount s - drugCount s

statShow :: String -> (String -> Int) -> String -> String
statShow s f x = column (s ++ ":") ++ (show . f) x ++ "\n"

column :: String -> String
column x = take 8 $ x ++ repeat ' '

-- Duplicates --------------------

wordList :: String -> Exp
wordList "" = [[""]]
wordList s  = map words (lines s)

dupes :: Exp -> Dupes
dupes l = [x `dupeCheck` y | (x:ys) <- tails l, y <- ys]

dupeCheck :: Drugs -> Drugs -> Dupe
dupeCheck a b | a == [] && b == []             = f
              | null (a \\ b) && null (b \\ a) = t
              | otherwise                      = f
              where t = Dupe True (unwords a) (unwords b)
                    f = Dupe False "" ""

onlyDupes :: Dupes -> Dupes
onlyDupes l = filter ((== True) . exists) l

pretty :: Dupes -> String
pretty [] = "No Duplicates\n\n"
pretty x  = foldr (\a b -> s a b) "" x
    where s a b = intercalate "\n" [ first a
                                   , second a
                                   , ""
                                   , b]
