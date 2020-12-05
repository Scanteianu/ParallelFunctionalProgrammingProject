module DocumentIndexing where
  import Data.Char
  import Data.List
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set
  -- code is stolen liberally from my hws -ds

  --high level pipeline illustration: read files into strings -> turn strings into docs (parallel) -> get all words from doc collection (singlethreaded for now) -> compute tfidf map for each document (parallel)->ready to search!

  -- All the information we need to know about a given document
  data Document = Document { text::String, termFrequency :: Map.Map String Int , termSet :: Set.Set String , tfidf::Map.Map String Double }

  --theft from hw
  printFields :: [a -> String] -> a -> String
  printFields functs object = intercalate " " $ map ($ object) functs

  instance Show Document where
    show = printFields [show . text, show . termFrequency, show . termSet, show . tfidf]

  --this is the thing that we'll need to parallelize; figure out how to do parmap here
  singleThreadedReadDocuments :: [String] -> [Document]
  singleThreadedReadDocuments docs = map readDocument docs
  --basic text->document parsing (note, tfidf has to be added as a second step)
  readDocument :: String -> Document
  readDocument text = Document text wordMap (Map.keysSet wordMap) Map.empty
      where
          wordMap = Map.fromList (countTuples text)
  --this thing can also be parallelized
  updateDocumentsWithTfIdfScore :: [Document] -> Map.Map String Int -> [Document]
  updateDocumentsWithTfIdfScore docs globTermFreq = map (`updateDocWithTfIdf` globTermFreq) docs

  --individual doc tfidf computation
  updateDocWithTfIdf :: Document -> Map.Map String Int -> Document
  updateDocWithTfIdf doc wordMap = Document (text doc) (termFrequency doc) (termSet doc) (Map.intersectionWith (\x y ->(fromIntegral x)/(fromIntegral y)) (termFrequency doc) wordMap)
  -- this is the
  getGlobalDocumentFrequency :: [Document] -> Set.Set String-> Map.Map String Int
  getGlobalDocumentFrequency [] wordSet = Map.fromSet (\x -> 0) wordSet
  getGlobalDocumentFrequency (x:xs) wordSet = Set.foldl updateWithWord (getGlobalDocumentFrequency xs wordSet) (termSet x)
  --helper functions below here
  updateWithWord :: Map.Map String Int -> String -> Map.Map String Int
  updateWithWord wordMap word = Map.adjust (+1) word wordMap

  getAllTheWords :: [Document] -> Set.Set String
  getAllTheWords [] = Set.empty
  getAllTheWords (x:xs) = Set.union (termSet x) (getAllTheWords xs)


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
