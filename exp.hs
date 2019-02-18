import Control.Applicative (liftA2)
import Data.Char (toUpper)
import Data.List ((\\), maximumBy, find, intercalate, sort, tails)
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.IO (writeFile)

main = do
    args <- getArgs
    if ["-h", "--help"] `anyEq` args
        then do
            help <- readmeGet
            putStr help
        else if ["-s", "--sort"] `anyEq` args
                then do
                    putStrLn "Sort the experience file alphabetically?"
                    sortConfirmation <- getLine
                    home <- getHomeDirectory
                    exp  <- expGet ["/home/bengyup/Desktop/backup/exp.txt"]
                    let theLines = sort $ lines exp
                    let firstWord = head $ words $ head theLines
                    let lastWord = last $ words $ last theLines
                    let alphabetised = unlines theLines
                    putStr alphabetised
                    writeFile "/home/bengyup/Desktop/backup/exp.txt" alphabetised
                    let msg = "File has been sorted alphabetically from " ++ sortedExample firstWord ++ " to " ++ sortedExample lastWord
                    putStrLn msg
                else do
                    home <- getHomeDirectory
                    let expPath = [ "exp.txt", home ++ "exp.txt"]
                    exp  <- expGet $ args ++ expPath
                    heading "stats"
                    let totalShow = statShow "Total" totalCount exp
                    putStr $  statShow "Drugs"  drugCount  exp
                           ++ statShow "Combos" comboCount exp
                           ++ totalLine totalShow
                           ++ totalShow
                           ++ "\n"
                           ++ longestShow exp
                    heading "duplicates"
                    putStr $ pretty
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

helpMsg :: String
helpMsg = "See help: [-h] or [--help]"

emDash, triangle, openCurly, closeCurly :: Char

emDash = '\8212'
triangle = '\x25b3'
openCurly = '\x2018'
closeCurly = '\x2019'

surroundCurly, addEllipsis, sortedExample, errFormat, column :: String -> String

surroundCurly s = [openCurly] ++ s ++ [closeCurly]
addEllipsis s = s ++ " ..."
sortedExample = surroundCurly . addEllipsis
lineLength = take 40
errFormat s = nn ++ t ++ nn ++ s ++ nn ++ t ++ "\n"
    where nn = "\n\n"
          t  = lineLength $ unwords $ repeat $ triangle : ' ' : ""
column x = take 8 $ x ++ repeat ' '

unlines' :: [String] -> String
unlines' = intercalate "\n"

anyEq :: Eq a => [a] -> [a] -> Bool
anyEq x y = any id $ liftA2 (==) x y

indent :: String
indent = replicate 2 ' '

heading :: String -> IO ()
heading s = putStrLn $ "\n" ++ x ++ "\n"
    where x = lineLength $ (map toUpper s) ++ " " ++ repeat '='

readmeGet :: IO String
readmeGet = do
    let r = "README.md"
    exists <- doesFileExist r
    cd     <- getCurrentDirectory
    if exists
        then readFile r
        else error $ errFormat $ unlines' [ r ++ " not found in:"
                                          , cd
                                          , helpMsg
                                          ]

expGet :: [FilePath] -> IO String
expGet []     = error $ errFormat $ "No experiences file found.\n" ++ helpMsg
expGet (x:xs) = do
    exists <- doesFileExist x
    if exists then readFile x else expGet xs

wordList :: String -> Exp
wordList "" = [[""]]
wordList s  = map words (lines s)

-- Stats --------------------

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = (if x `elem` xs then id else (x:)) $ unique xs

drugCount, totalCount :: String -> Int

drugCount = length . unique . words
totalCount = length . lines

totalLine :: String -> String
totalLine = map (\x -> if x == '\n' then x else emDash)

comboCount :: String -> Int
comboCount s = totalCount s - drugCount s

longest :: (Foldable t1, Foldable t2) => t1 (t2 a) -> t2 a
longest = maximumBy (comparing length)

longestCombo :: String -> [String]
longestCombo = longest . wordList

longestCount :: String -> Int
longestCount = length . longestCombo

statShow :: String -> (String -> Int) -> String -> String
statShow s f x = column (s ++ ":") ++ (show . f) x ++ "\n"

longestShow :: String -> String
longestShow s = "Longest combo (most drugs at once):\n"
                ++ subItem "Length:" (show . longestCount)
                ++ subItem "Combo:"  (unwords . longestCombo)
                    where subItem t f = indent ++ column t ++ f s ++ "\n"

-- Duplicates --------------------

dupes :: Exp -> Dupes
dupes l = [x `dupeCheck` y | (x:ys) <- tails l, y <- ys]

dupeCheck :: Drugs -> Drugs -> Dupe
dupeCheck a b | a == [] && b == []             = f
              | null (a \\ b) && null (b \\ a) = t
              | otherwise                      = f
              where t = Dupe True  (unwords a) (unwords b)
                    f = Dupe False ""          ""

onlyDupes :: Dupes -> Dupes
onlyDupes l = filter ((== True) . exists) l

pretty :: Dupes -> String
pretty [] = "No Duplicates\n\n"
pretty x  = foldr (\a b -> s a b) "" x
    where s a b = unlines' [ first a
                           , second a
                           , ""
                           , b]
