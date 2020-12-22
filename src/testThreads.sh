for i in {1..4}
do
    echo "Time: maxSort"
    echo "Thread $i"
    for j in {1..10}
    do
        ./Tfidf +RTS -N$i -RTS -f "../SampleTestFiles/100ktwitter.txt" "airline delay" 4
        wait
    done
done

for i in {1..4}
do
    echo "Time: maxSort"
    echo "Thread $i"
    for j in {1..10}
    do
        ./Tfidf +RTS -N$i -RTS -f "../SampleTestFiles/500ktwitter.txt" "airline delay Maybe Facebook weather" 4
        wait
    done
done


for i in {1..4}
do
     echo "Time: maxSort"
    echo "Thread $i"
    for j in {1..10}
    do
        ./Tfidf +RTS -N$i -RTS -f "../SampleTestFiles/1mtwitter.txt" "airline delay Twitter Why Always good" 4
        wait
    done
done
