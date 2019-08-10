Testing APE
===========

Author: Kaarel Kaljurand

Version: 2019-08-09

Introduction
------------

This document explains how to test APE including the various DRS translators
that APE contains.

All the test scripts in this directory depend on a larger lexicon than included
with APE. In order to download the required lexicon, first run

    swipl -f none -g ensure_clex -t halt -s downloader.pl


### APE Regression test

To regression test APE, just run:

    bash rtest.bash

or if you want to download the latest regression testset first
then run:

    swipl -f none -g download_acetexts -t halt -s downloader.pl

A file with a timestamp containing the regression test results
along with APE's output is stored into the `testruns`-directory.

In order to get a digest of the testrun, grep the file
for regression tester messages, e.g.:

    cat testruns/rtest_050601-1310.txt | grep "^0"

or cat an already filtered file:

    cat tmp/now.txt

In order to explore the erronous DRSes, open the file and search
for the strings

  - `0000` (Correctly generated an empty DRS)
  - `----` (Correctly generated a DRS which is not empty)
  - `0->#` (Failed to generate an empty DRS)
  - `#->0` (Incorrectly generated an empty DRS)
  - `####` (DRSs which are not empty do not match.)

To get a listing of all the regressions:

    cat tmp/now.txt | grep "\[.*#.*\]"

Note: the regression testing is only tested with SWI-Prolog.
It is assumed that SWI-Prolog is called `swipl`,
if it is not the case then modify `rtest.bash` accordingly
or set a symbolic link, e.g.:

    ln -s `which pl` swipl

Runtime of the complete test run on different machines:

  - Mac OS X G4: ~25 seconds (~3000 testcases)
  - Intel i3, 2011-06-20: ~11 seconds (3613 test cases, excluding bug reports)
  - Intel i7, 2016-04-02: ~9 seconds (3733 test cases, excluding bug reports, including loading the large lexicon from a flat file)


### Testing Drace Core and Drace NP

Running the test:

    echo "[test_drace]. test_drace(core)." | swipl > testruns/drace_test_results.txt
    echo "[test_drace]. test_drace(np)." | swipl > testruns/dracenp_test_results.txt

Getting an overview of the problems by filtering out certain testcases
that point to pseudo-problems.

    cat testruns/drace_test_results.txt | grep "FAIL" | grep -v ":" | grep -v "ach of" | wc


### Running all the tests

    time bash test_everything.sh > out.txt 2> err.txt

This takes about

  - 55 seconds on an i3 Linux laptop with SWI-Prolog v6.
  - 45 seconds on an i7 Linux laptop with SWI-Prolog v7.
