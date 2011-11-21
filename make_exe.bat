@echo off
del ape.exe
echo Trying "plcon" as SWI Prolog command...
plcon -O -F none -g "working_directory(_, 'parser'), [fit_to_plp], halt." -t halt & plcon -O -f ape.pl -g "qsave_program('ape.exe', [goal(ape), toplevel(halt), local(25000), global(50000)])." -t halt
if exist ape.exe goto success
echo "plcon" did not succeed. Trying "swipl"...
swipl -O -F none -g "working_directory(_, 'parser'), [fit_to_plp], halt." -t halt & swipl -O -f ape.pl -g "qsave_program('ape.exe', [goal(ape), toplevel(halt), local(25000), global(50000)])." -t halt
if exist ape.exe goto success
echo "swipl" did not succeed either. Could not find the SWI Prolog command.
echo Make sure that the location of the SWI Prolog binaries is in the PATH variable.
goto end
:sucess
echo ape.exe created.
:end