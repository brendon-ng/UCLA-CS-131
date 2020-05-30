#! /bin/bash

threads=(1 8 20 40)
size=(1 5 50 100)
class=("Null" "Synchronized" "Unsynchronized" "AcmeSafe")


for c in ${class[@]}
do
    for s in ${size[@]}
    do
	for t in ${threads[@]}
	do
	    printf "$c $s $t " >> lnxsrv10.txt
	    str=`time timeout 3600 java UnsafeMemory $c $t 100000000 $s 2>&1`
	    echo $str | grep -o "[0-9]*\.[0-9]*" | sed -n '1,4p' | tr '\n' ' ' >> lnxsrv10.txt
	    if echo $str | grep -e "mismatch"; then
		echo " 1" >> lnxsrv10.txt
	    else
		echo " 0" >> lnxsrv10.txt
	    fi
	done
    done
done

echo "TEST FINISHED RESULTS IN data.txt"

# time timeout 3600 java UnsafeMemory AcmeSafe 8 100000 5 | grep -o "[0-9]\.[0-9]*" | sed -n '1,4p' | tr '\n' ' ' > test.txt
