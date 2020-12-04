import DocumentIndexing

testTokenize = tokenizeAndNormalize "hello world world hello WORLD worldly hello foobar"
testTuples = countTuples "hello world world hello WORLD worldly hello foobar"
