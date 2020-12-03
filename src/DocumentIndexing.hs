module DocumentIndexing where
  import Data.Char
  --return a list of normalized tokens
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
