import Data.List (sortBy, nub)
import Data.Ord (comparing)

wordCount :: FilePath -> FilePath -> IO ()
wordCount inputF outputF = do
    content <- readFile inputF
    let wordsList = words content
    let uniqueWords = nub wordsList
    let wordsFrequencies = map (\x -> (x, length (filter (== x) wordsList))) uniqueWords
    let sortedWordsFrequencies =  sortBy (flip (comparing snd)) wordsFrequencies
    writeFile outputF (unlines (map (\(w, n) -> w ++ ": " ++ show n) sortedWordsFrequencies))

main :: IO ()
main = do
    wordCount "input.txt" "output.txt"
