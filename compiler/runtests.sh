#!/bin/bash

# FILENAME :  runtests.sh
# AUTHOR(S):  Joe Lee (jyl2157)
# PURPOSE  :  Shell script to run tests on executable.
#             Each test in the tests dir is run on the executable
#             with its standard out piped to a .out file.  The .out
#             file is compared with a corresponding .gs (gold standard)
#             file for each test.  If the test fails (output differs),
#             a .diff file is created for developer use.

APP=$(dirname $0)/ezac
globallog=test_ezac.log
testdir=tests
rm -f $globallog
error=0

Check() {
	basename=$(basename $1) 
	basename=${basename%.*}
	  
	ezafile=$testdir/${basename}.eza
	reffile=$testdir/${basename}.gs
	outfile=$testdir/${basename}.out
	difffile=$testdir/${basename}.diff

	echo -n "Running $basename..."
	  
	$APP $ezafile > $outfile 2>&1 || {
		echo "Failed: ezac terminated."
	  	error=1; 
		return 1
	}

	diff -b $reffile $outfile > $difffile 2>&1 || {
		echo "Failed: output mismatch."
		error=1; 
		return 1
	}      	

	rm $outfile $difffile
	echo "OK."
}

for file in $testdir/test*.eza
do
	  Check $file 2>> $globallog
done

exit $error
