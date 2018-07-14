# APE regression tester
#
# Usage:
#
# bash rtest.bash
# bash rtest.bash d
#
# @author Kaarel Kaljurand
# @version 2011-07-28
#
# If the commandline argument `d' is present then the latest version of
# the regression testset is downloaded from the web.
# Note that `curl' or `wget' must be installed to be able to download the regression testset.
# In case you don't have `curl' or `wget',
# download the testset manually before running the script (without `d', in this case).

prolog=swipl
#prolog=`which swipl`

#downloader='curl -o'
downloader='wget -O'

clex='clex_lexicon.pl'

echo "Using: `$prolog --version`"

# Generate a timestamp.
timestamp=`date '+%y%m%d-%H%M'`

# Download the latest testset.
if [ $# -eq 1 ]
then
if [ $1 = "d" ]
then
echo "Downloading the latest ACE text set ... "
$downloader acetexts.pl http://attempto.ifi.uzh.ch/cgi-bin/acetextset/get_acetexts.cgi
echo "done."
fi
fi


if [ ! -f $clex ]; then
	echo "Downloading the large Clex lexicon (from github.com/Attempto/Clex)"
	$downloader $clex https://raw.github.com/Attempto/Clex/master/$clex
fi

# Creates a directory for the test results.
# If the directory already exists then an error message is printed (which you can ignore).
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
