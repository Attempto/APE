/**
This is an example that shows how to

1. Translate an ACE file into a DRS
2. Verbalize this DRS (effectively paraphrasing the ACE text (in this case in Core ACE))
3. Translate the resulting paraphrase into a new DRS
4. Test if the original DRS is structurally equivalent to the new DRS

@author Kaarel Kaljurand
@version 2009-06-16

*/

% We point to the directory where APE modules and the lexicons are located.
:- assert(user:file_search_path(ape, '..')).

% We import the needed modules.
:- use_module(ape(parser/ace_to_drs), [acetext_to_drs/8]).

:- style_check(-discontiguous).
:- use_module(ape(lexicon/clex)).
:- use_module(ape(lexicon/ulex)).
:- style_check(+discontiguous).

% Various modules that deal with the DRSs are located in ape/utils/.

:- use_module(ape(utils/drs_to_ascii), [drs_to_ascii/2]).

:- use_module(ape(utils/drs_to_coreace), [bigdrs_to_coreace/2]).

:- use_module(ape(utils/are_equivalent), [are_equivalent/3]).

% Various modules that deal with ACE texts (e.g. pretty-printing) are located in ape/utils/.

:- use_module(ape(utils/morphgen), [acesentencelist_pp/2]).


% This example uses the ACE version of the LOL policy.
acetext('the_lol_policy.ace.txt').

paraphrase_roundtrip :-
	acetext(AceText),
	% We load the file into character codes.
	read_file_to_codes(AceText, AceTextCodes, [encoding(utf8)]),
	format("ACE text = ~n~s~n", [AceTextCodes]),
	% We call APE with guessing turned "on".
	% The input is the character codes.
	% The output that interests us is the DRS and possible error/warning messages.
	acetext_to_drs(AceTextCodes, on, off, _Sentences1, _SyntaxTrees1, Drs1, Messages1, _DurationList1),
	% We pretty-print the DRS.
	drs_to_ascii(Drs1, DrsAscii1),
	format("DRS1 = ~n~w~n", [DrsAscii1]),
	format("Messages1 = ~n~w~n", [Messages1]),
	% We verbalize the DRS in Core ACE (the verbalizer expects untyped DRSs as input).
	bigdrs_to_coreace(Drs1, Paraphrase),
	% The verbalization results in a list of sentences where each sentence is a Prolog atom.
	% We convert the list structure into an atom.
	acesentencelist_pp(Paraphrase, ParaphrasePp),
	format("Paraphrase = ~n~w~n", [ParaphrasePp]),
	% We call APE again. This time on the paraphrase.
	% Note that the input is given now as an atom, not in character codes like before,
	% APE accepts both types of inputs.
	acetext_to_drs(ParaphrasePp, on, off, _Sentences2, _SyntaxTrees2, Drs2, Messages2, _DurationList2),
	(
		% We now test if the DRSs are structurally equivalent.
		% We ignore the sentence IDs while checking for equivalence.
		are_equivalent(Drs1, Drs2, [ignore_sid(true)])
	->
		% If the DRSs were equivalent then the roundtrip finished where it started.
		format("Roundtrip worked: ACE -> DRS -> Core ACE -> DRS~n", [])
	;
		% If the DRSs were not equivalent then the roundtrip did not take place.
		% This means that a bug was encountered in either the verbalizer
		% or in APE. We show the new DRS in this case, along with possible messages.
		format("Roundtrip did not work: ACE -> DRS1 -> Core ACE -> DRS2~n", []),
		drs_to_ascii(Drs2, DrsAscii2),
		format("DRS2 = ~n~w~n", [DrsAscii2]),
		format("Messages2 = ~n~w~n", [Messages2])
	).
