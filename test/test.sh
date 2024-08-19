echo "-------------------------------"

PROGRAM=./oeval

# Test each mode
for VAR in simplify evaluate derivate convert
do
    echo "Test $VAR"
	$PROGRAM --file test/test_$VAR.in > test/test_$VAR.out
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

# Test definitions
echo "Test definitions"
$PROGRAM --file test/test_defs.in --load test/defs.def > test/test_defs.out
DIFF=$(diff test/test_defs.out test/test_defs.exp)
if [ "$DIFF" ]
then
    echo "$DIFF"
else
    echo "Test OK"
fi
echo "-------------------------------"
rm test/test_defs.out