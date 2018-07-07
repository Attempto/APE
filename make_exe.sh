#!/bin/sh

( echo "Looking for command 'swipl'..." ; swipl -O -F none -g "working_directory(_, 'parser'), [fit_to_plp], halt." -t halt ; swipl -O -f ape.pl -g "qsave_program('ape.exe', [goal(ape), toplevel(halt)])." -t halt ) || echo "Error: SWI Prolog not found"
