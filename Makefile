# Unofficial Makefile for some tasks
# Tested only on Ubuntu Linux.

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
	@echo "    doc: generates documentation"
	@echo "  clean: deletes automatically generatable files"
	@echo "   test: runs some tests"
	@echo


build:
	swipl -O -F none -g "working_directory(_, 'parser'), [fit_to_plp], halt." -t halt ; swipl -O -f ape.pl -g "qsave_program('ape.exe', [goal(ape), toplevel(halt), local(25000), global(50000)])." -t halt

clean:
	rm -f *.exe *.toc

doc:
	cat parser/grammar.fit parser/grammar_functionwords.fit parser/grammar_contentwords.fit | perl parser/make_syntax_report.perl
	cat parser/grammar.fit parser/grammar_functionwords.fit parser/grammar_contentwords.fit | perl parser/make_syntax_report.perl > syntax_report.html

test:
	./ape.exe -text $(text1) -cdrspp -cparaphrase -cowlfsspp -csyntax -csyntaxpp -csyntaxd -csyntaxdpp
	./ape.exe -text $(text2) -cdrspp -cparaphrase -cowlfsspp
	./ape.exe -text $(text3) -cdrspp -cparaphrase
	./ape.exe -text $(text4) -solo owlfsspp
	./ape.exe -text $(text5) -cdrspp -cparaphrase -cowlfsspp -csyntax -csyntaxpp -csyntaxd -csyntaxdpp
