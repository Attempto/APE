# Runs all the APE tests in one go.
#
# Note that these tests overwrite files that are under version
# control. In case the only change in the test results is random
# variable names or IDs, then revert the change by
#
# git checkout .
#

pl=swipl

dir=testruns

results_coreace="${dir}/drace_test_results.txt"
results_npace="${dir}/dracenp_test_results.txt"
results_owlswrl="${dir}/owlswrl_test_results.txt"
results_tptp="${dir}/test_drs_to_tptp_out.txt"

echo
echo "ACE -> DRS"
echo

bash rtest.bash


echo
echo "DRS -> Core ACE"
echo

echo "[test_drace]. test_drace(core)." | $pl > ${results_coreace}


echo
echo "DRS -> NP ACE"
echo

echo "[test_drace]. test_drace(np)." | $pl > ${results_npace}


echo
echo "DRS -> OWL/SWRL"
echo

./test_owlswrl.pl > ${results_owlswrl}


echo
echo "DRS -> TPTP"
echo

echo "[test_drs_to_x]. main." | $pl > ${results_tptp}
