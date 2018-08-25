# Tested only on Ubuntu Linux.
# For Windows builds use make_exe.bat

SHELL=/bin/bash

swipl = swipl
version = 6.7

text1 = "John likes Mary."
text2 = "Every man likes a car."
text3 = "1 / 2 ^ 3 ^ 4 = 1/8."
text4 = "If John likes Mary then 1 / 2 ^ 3 = 1/8."
text5 = "1 / 2 * 3 - 4 = 1/8."

all: help

help:
	@echo Targets:
	@echo
	@echo "  build: builds ape.exe"
	@echo "install: (same as build)"
	@echo "    doc: generates documentation"
	@echo "  clean: deletes automatically generatable files"
	@echo "   test: runs some tests"
	@echo


build:
	$(swipl) -O -F none -g "working_directory(_, 'prolog/parser'), [fit_to_plp], halt." -t halt ; $(swipl) -O -f ape.pl -g "qsave_program('ape.exe', [goal(ape), toplevel(halt)])." -t halt

install: build

check:

clean:
	rm -f *.exe *.toc prolog/parser/*.{plp,html,toc}

doc:
	cat prolog/parser/{grammar,grammar_functionwords,grammar_contentwords}.fit | perl prolog/parser/make_syntax_report.perl
	cat prolog/parser/{grammar,grammar_functionwords,grammar_contentwords}.fit | perl prolog/parser/make_syntax_report.perl --number $(version) > syntax_report.html

test:
	./ape.exe -text $(text1) -cdrspp -cparaphrase -cowlfsspp -csyntax -csyntaxpp -csyntaxd -csyntaxdpp
	./ape.exe -text $(text2) -cdrspp -cparaphrase -cowlfsspp
	./ape.exe -text $(text3) -cdrspp -cparaphrase
	./ape.exe -text $(text4) -solo owlfsspp
	./ape.exe -text $(text5) -cdrspp -cparaphrase -cowlfsspp -csyntax -csyntaxpp -csyntaxd -csyntaxdpp
