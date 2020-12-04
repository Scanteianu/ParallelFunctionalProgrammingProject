module DocumentIndexing where
  import Data.Char
  import Data.List



  -- the below will create a set of tuples where the first element is the word, and the second is how many times it shows up
  countTuples :: String -> [(String, Int)]
  countTuples text = countWords (sort (tokenizeAndNormalize text)) []

  countWords :: [String]->[(String,Int)] -> [(String,Int)]
  countWords a b
      | a== [] = b
      | b==[] = countWords (tail a) (((head a),1):b)
      | head a == fst (head b) = countWords (tail a) updateB
      | otherwise = countWords (tail a) (((head a),1):b)
      where
      updateB = (fst(head b),snd(head b)+1):(tail b)
  --return a list of normalized tokens; function and helpers
  tokenizeAndNormalize::String -> [String]

  tokenizeAndNormalize doc = splitString doc ""

  splitString :: String -> String -> [String]
  splitString text current
      | text == "" && current =="" =[]
      | text =="" && (sanitizeWord current)=="" = []
      | text =="" = [current]
      | isSpace (head text) && current=="" = splitString (tail text) current
      | isSpace (head text) && (sanitizeWord current)=="" = splitString (tail text) ""
      | isSpace (head text) = sanitizeWord(current) : (splitString (tail text) "")
      | otherwise = splitString (tail text) (current++[(head text)])

  sanitizeWord:: String -> String
  sanitizeWord word = map toLower (filter isLetter word)
