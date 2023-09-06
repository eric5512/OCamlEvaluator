echo "-------------------------------"
for VAR in simplify evaluate derivate
do
    echo "Test $VAR"
	./oeval.native --mode $VAR --file test/test_$VAR.in > test/test_$VAR.out
	DIFF=$(diff test/test_$VAR.out test/test_$VAR.exp)
    if [ "$DIFF" ]
    then
        echo "$DIFF"
    else
        echo "Test OK"
    fi
    echo "-------------------------------"
    rm test/test_$VAR.out
done