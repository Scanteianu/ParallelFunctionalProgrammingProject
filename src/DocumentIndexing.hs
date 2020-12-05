module DocumentIndexing where
  import Data.Char
  import Data.List
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set
  import Data.Maybe
  -- code is stolen liberally from my hws -ds

  --high level pipeline illustration: read files into strings -> turn strings into docs (parallel) -> get all words from doc collection (singlethreaded for now) -> compute tfidf map for each document (parallel)->ready to search!

  -- All the information we need to know about a given document
  data Document = Document { text::String, termFrequency :: Map.Map String Int , termSet :: Set.Set String , tfidf::Map.Map String Double }

  --theft from hw
  printFields :: [a -> String] -> a -> String
  printFields functs object = intercalate " " $ map ($ object) functs

  instance Show Document where
    show = printFields [show . text, show . termFrequency, show . termSet, show . tfidf]

  --user friendly output
  simplifyOutput :: [(Document,Double)] -> [(String, Double)]
  simplifyOutput inputs = [(text a, b)|(a,b)<-inputs]

  --this is the search function - input is a string of space separated keywords and the list of docs, output is the docs sorted by score and scores, in tuples
  -- yet again, the map thing here can become parallel
  searchAndSort:: String -> [Document] -> [(Document,Double)]
  searchAndSort keywords docs = sortDocsByScore (map (docScore (tokenizeAndNormalize keywords)) docs)
  docScore :: [String] -> Document -> (Document, Double)
  docScore keywords doc = (doc, foldl (+) 0 [keywordScore x doc | x <- keywords])

  keywordScore :: String -> Document -> Double
  keywordScore kw doc = fromMaybe 0 (Map.lookup kw (tfidf doc)) --https://hoogle.haskell.org/?hoogle=fromMaybe
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

  -- this is the thing that gets the global count of docs each word appears in
  getGlobalDocumentFrequency :: [Document] -> Set.Set String-> Map.Map String Int
  getGlobalDocumentFrequency [] wordSet = Map.fromSet (\x -> 0) wordSet
  getGlobalDocumentFrequency (x:xs) wordSet = Set.foldl updateWithWord (getGlobalDocumentFrequency xs wordSet) (termSet x)

  sortDocsByScore ::Ord a => [(Document, a)]->[(Document, a)]
  sortDocsByScore scoredDocs = sortBy (\(_,a) (_,b) -> compare b a) scoredDocs
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
