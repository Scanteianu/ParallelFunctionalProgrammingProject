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
    for i in range(1,10):
        elapsed=runAndTime(threads,file)
        elapsedList.append(elapsed)
        details+="{},{}\n".format(threads, elapsed)
    return numpy.mean(elapsedList), numpy.std(elapsedList),details

print("timing!")
details=""

details+="100k:\n"
print("100k:")
for i in range(1,6):
    elapsed,stdev,details=runAndStat(i,"../SampleTestFiles/100ktwitter.txt",details)
    print("{},{},{}".format(i, elapsed,stdev),flush=True)
    
details+="500k:\n"
print("500k:")
for i in range(1,6):
    elapsed,stdev,details=runAndStat(i,"../SampleTestFiles/500ktwitter.txt",details)
    print("{},{},{}".format(i, elapsed,stdev),flush=True)
    
    
details+="1m:\n"
print("1m:")
for i in range(1,6):
    elapsed,stdev,details=runAndStat(i,"../SampleTestFiles/1mtwitter.txt",details)
    print("{},{},{}".format(i, elapsed,stdev),flush=True)
    
    
details+="1.5m:\n"
print("1.5m:")
for i in range(1,6):
    elapsed,stdev,details=runAndStat(i,"../SampleTestFiles/15mtwitter.txt",details)
    print("{},{},{}".format(i, elapsed,stdev),flush=True)


    
print(details,flush=True)