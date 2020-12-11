import DocumentIndexing
import Data.Set as Set

testTokenize = tokenizeAndNormalize "hello world world hello WORLD worldly hello foobar"
testTuples = countTuples "hello world world hello WORLD worldly hello foobar"
testRead = readDocument "hello world world hello WORLD worldly hello foobar"
testStRead = singleThreadedReadDocuments ["hello world world hello WORLD worldly hello foobar", "the foobar cat jumped over the worldly dog", "the fox wears socks", "sculptures wear hard wear but people wear soft wear", "wear the wild things be"]
testAllWords = getAllTheWords testStRead
testGlobFreqBC = getGlobalDocumentFrequency [] testAllWords --bc==basecase
testUWW =  updateWithWord testGlobFreqBC "hello"
scratchGlobFreq = Set.foldl updateWithWord testGlobFreqBC (Set.fromList ["hello","foobar"])
testGlobFreq = getGlobalDocumentFrequency testStRead testAllWords
testFullIndex = updateDocumentsWithTfIdfScore testStRead testGlobFreq
testScores = zip testFullIndex [1..10]
testSearch = simplifyOutput (searchAndSort "hello fox" testFullIndex)
testSearchSeq = fmap simplifyOutput (searchAndSortSequential "hello fox" testFullIndex)
testSearchPar = fmap simplifyOutput (searchAndSortSequential "hello fox" testFullIndex)
