#!/bin/bash
echo "-------------------------------"

# Get the directory where the script is located
SCRIPT_DIR=$(dirname "$0")

# Change to the script directory to make relative paths work
cd ../../../test

# Now, PROGRAM and all file paths are relative to the script's location
PROGRAM="../_build/install/default/bin/oeval"

# Test each mode
for VAR in simplify evaluate derivate convert base
do
    echo "Test $VAR"
    $PROGRAM --file "./test_$VAR.in" > "./test_$VAR.out"
    DIFF=$(diff "./test_$VAR.out" "./test_$VAR.exp")
    if [ "$DIFF" ]
    then
        echo "$DIFF"
    else
        echo "Test OK"
    fi
    echo "-------------------------------"
    rm "./test_$VAR.out"
done

# Test definitions
echo "Test definitions"
$PROGRAM --file "./test_defs.in" --load "./defs.def" > "./test_defs.out"
DIFF=$(diff "./test_defs.out" "./test_defs.exp")
if [ "$DIFF" ]
then
    echo "$DIFF"
else
    echo "Test OK"
fi
echo "-------------------------------"
rm "./test_defs.out"
