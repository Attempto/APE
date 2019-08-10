# APE regression tester
#
# Usage:
#
# bash rtest.bash
#
# @author Kaarel Kaljurand
# @version 2019-08-10

prolog=swipl
#prolog=`which swipl`

echo "Using: `$prolog --version`"

# Generate a timestamp.
timestamp=`date '+%y%m%d-%H%M'`

# Ensures a directory for the test results.
mkdir -p testruns/

# Convert fit-files into plp-files.
$prolog -g "working_directory(_, '../prolog/parser'), [fit_to_plp], halt."

# Run the regression test.
#time echo "[test_ape]. main. halt." | $prolog -q > testruns/rtest_$timestamp.txt
time $prolog -f test_ape.pl -g main -t halt -q > testruns/rtest_$timestamp.txt

echo ""
echo "Regression tester finished."
echo "The results of the regression test were saved into testruns/rtest_$timestamp.txt"

echo ""
echo "Diff with the previous version:"
echo ""

mkdir -p tmp/
ls testruns/rtest*.txt | tail -2 | head -1 | xargs grep "^0" > tmp/before.txt
ls testruns/rtest*.txt | tail -1 | xargs grep "^0" > tmp/now.txt
diff tmp/before.txt tmp/now.txt

echo ""
echo "Finished."
