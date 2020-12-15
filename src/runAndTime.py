import subprocess
import time
import numpy

def runAndTime(threads, file):
    start=time.time()
    out=subprocess.run(["Tfidf",'+RTS','-N'+str(threads) ,'-RTS', '-f', file, "airline delay"],capture_output=True)
    end=time.time()
    #print(end-start)
    #print(out)
    return end-start
def runAndStat(threads,file,details):
    elapsedList=[]
    for i in range(1,2):
        elapsed=runAndTime(threads,file)
        elapsedList.append(elapsed)
        details+="{},{}\n".format(threads, elapsed)
    return numpy.mean(elapsedList), numpy.std(elapsedList),details

print("timing!")
details=""

details+="large:\n"
print("large:")
for i in range(1,9):
    elapsed,stdev,details=runAndStat(i,"../SampleTestFiles/bigtwitter.txt",details)
    print("{},{},{}".format(i, elapsed,stdev),flush=True)

details+="small:\n"
print("small:")
for i in range(1,9):
    elapsed,stdev,details=runAndStat(i,"../SampleTestFiles/twitterCustomerSupportTruncated.txt",details)
    print("{},{},{}".format(i, elapsed,stdev),flush=True)

details+="long:\n"
print("long:")
for i in range(1,9):
    elapsed,stdev,details=runAndStat(i,"../SampleTestFiles/twitterLargeStrings.txt",details)
    print("{},{},{}".format(i, elapsed,stdev),flush=True)
    
print(details,flush=True)