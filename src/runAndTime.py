import subprocess
import time

def runAndTime(threads, file):
    start=time.time()
    out=subprocess.run(["Tfidf",'+RTS','-N'+str(threads) ,'-RTS', '-f', file, "airline delay"],capture_output=True)
    end=time.time()
    #print(end-start)
    #print(out)
    return end-start

print("timing!")

print("large:")
for i in range(1,5):
    elapsed=runAndTime(i,"../SampleTestFiles/twitterLargeStrings.txt")
    print("{} : {}".format(i, elapsed))

print("small:")
for i in range(1,5):
    elapsed=runAndTime(i,"../SampleTestFiles/twitterCustomerSupportTruncated.txt")
    print("{} : {}".format(i, elapsed))
