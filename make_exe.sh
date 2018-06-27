#!/bin/sh

( echo "Looking for command 'swipl'..." ; swipl -O -F none -g "working_directory(_, 'parser'), [fit_to_plp], halt." -t halt ; swipl -O -f ape.pl -g "qsave_program('ape.exe', [goal(ape), toplevel(halt), local(25000), global(50000)])." -t halt ) || ( echo "The command 'swipl' did not work. Let's try 'pl'..." ; pl -O -F none -g "working_directory(_, 'parser'), [fit_to_plp], halt." -t halt ; pl -O -f ape.pl -g "qsave_program('ape.exe', [goal(ape), toplevel(halt), local(25000), global(50000)])." -t halt ) || echo "Error: SWI Prolog not found"
