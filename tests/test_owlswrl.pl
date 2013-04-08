% Usage:
% $ swipl -f none -g main -t halt -s test_owlswrl.pl
% Writes the test results into STDIN and statistics into STDERR.

:- assert(user:file_search_path(ape, '../')).

:- use_module(ape('parser/ace_to_drs'), [
		acetext_to_drs/5
	]).

:- use_module(ape('utils/owlswrl/drs_to_owlswrl')).
:- use_module(ape('utils/owlswrl/owlswrl_to_xml'), [
		owlswrl_to_xml/2
	]).
:- use_module(ape('utils/owlswrl/owlswrl_to_fss'), [
		owlswrl_to_fss/1
	]).

:- use_module(ape('logger/error_logger'), [
		clear_messages/0,
		get_messages/1
	]). 

:- use_module(ape('utils/xmlterm_to_xmlatom'), [
		xmlterm_to_xmlatom/2
	]).


:- set_prolog_flag(float_format, '%.11g').

t('1-1', 'Every man is a human.').
t('1-2', 'Every man is somebody.').
t('1-3', 'Every man is something.').
t(2, 'Every human is a male or is a female.').
t(3, 'No dog is a cat.').
t(4, 'Every driver owns a car.').
t(5, 'Everything that a goat eats is some grass.').
t(6, 'John likes Mary.').
t(7, 'Everybody who loves somebody likes him/her.').
t(8, 'Every professor is a scientist and is a teacher.').
t(9, 'Everybody who is not a child is an adult.').
t(10, 'Nothing is a man and is a woman.').
t(11, 'If there is a woman then she does not like a snake.').
t(12, 'There is a woman who everybody likes.').

t(13, 'Every man likes at least 3 things.').
t(14, 'If a thing A is taller than a thing B then B is shorter than A.').
t(15, 'If something A is taller than something B and B is taller than something C then A is taller than C.').
t('15-1', 'If a thing A is taller than a thing that is taller than a thing C then A is taller than C.').

% SWRL
t('15-2', 'If a dog is taller than a cat that is taller than a mouse then the dog is taller than the mouse.').

t(16, 'Everybody who writes something is a human.').
t(17, 'Everybody who writes at least 1 thing is a human.').

t(18, 'Everything that somebody writes is a book or is a paper.').

t(19, 'Everything identifies at most 1 thing.').

t(20, 'If a thing A loves a thing B then B loves A.').

t(21, 'Nobody who likes a carrot is a carnivore.').

t(22, 'Every man likes at least 3 cars.').

t(23, 'Every man likes some cars.').

t(24, 'John likes every car.').

t(25, 'John likes no car.').

t(27, 'If there is a man then a dog likes the man.').

t(28, 'If there is a cat then at least 2 things like the cat.').

t(29, 'If there is a cat then at least 2 persons like the cat.').

t(30, 'For every thing at most 1 thing is identified by it.').

t(31, 'John likes Mary. Bill sees Mary.').

t(32, 'Every man is somebody that a dog likes.').

t(33, 'If a man sees a woman then the woman sees John.').

% SWRL
t(34, 'If a man sees a woman then a dog sees a cat.').

t(35, 'If a man sees a woman then John knows the man.').

t(36, 'Every man waits.').

t(37, 'Every man does not wait.').

t(38, 'Every man is an animal or does not wait.').

t(39, 'No man waits.').

% SWRL
t(40, 'If a man sees a dog then the man hears the dog.').

t(41, 'For every carnivore everything that the carnivore eats is a meat.').

t(42, 'Every man who sees a dog hears a cat that sees itself.').
t(43, 'Every man who sees a dog hears a cat that sees a mouse that hates the cat.').
t(44, 'Every man who sees a dog hears a cat that sees a mouse that hates itself.').
t(45, 'Every man who likes himself is strange.').
t(46, 'Every man likes himself.').

t(47, 'Every man who is not liked by a woman and who owns a dog sees a cat.').
t(48, 'If there is a man and it is false that the man is liked by a woman and that a dog is owned by the man then the man sees a cat.').

t(49, 'John\'s age is 30.').
t(50, 'John\'s address is "Poland".').
t(51, 'John is not Mary.').
t(52, 'It is false that a man sees a woman.').
t(53, 'Everybody who waits is a grown-up.').
t(54, 'Everybody whose age is 31 is a grown-up.').
t(55, 'Everybody whose address is "Poland" is a human.').
t(56, 'Everybody whose age is 31 and who waits is a grown-up.').
t(57, 'Every man likes no dog.').
t(58, 'Every student is John or is Mary.').
t(59, 'Everybody who is John or who is Mary is a student.').
t(60, 'John is a man or owns less than 3 cars.').
t(61, 'John does not like Mary.').
t(62, 'John does not own a car.').
t(63, 'John does not own more than 3 cars.').

t(64, 'A man is taller than more than 3 animals.').
t(65, 'A man is not a woman.').
t(66, 'Every carnivore eats every meat.').
t(67, 'Everybody likes everybody.').
t(68, 'John\'s brother likes everybody.').

t(69, 'If somebody X loves somebody Y then it is false that X hates Y.').

t(70, 'If a man likes somebody that is a person then the person owns a car.').
t(71, 'If John is a man then the man is a person.').

t(72, 'If a man owns a dog and the man owns a cat and the dog likes the cat then the man is a human.').
t(73, 'Every man is at most 3 cars.').

t(74, 'Mary is liked by nobody.').

t(75, 'Every man is something that likes something that owns a car and that likes Mary.').

t(76, 'Every man is something that likes a car and that likes a bike.').

t(77, 'No man is something that likes a car and that likes a bike.').

t(78, 'If there is a goat and everything that the goat eats is an apple then the goat is an animal.').

t(79, 'If there is a goat and everything that the goat eats is not an apple then the goat is an animal.').

t(80, 'Mary likes a cat. Every man likes the cat.').

t(81, 'John does not like every dog.').

t(82, 'John\'s brother likes Mary. An age of the brother is 10.').

t(83, 'John is something that is not Mary.').

t(84, 'John is not something that is Mary.').

t(85, 'Everything that likes something that sees something that hears something hates it.').

t(86, 'Every man owns exactly 3 cars.').

t(87, 'Every man owns exactly 3 things.').

t(88, 'If a man likes a dog that likes a cat and the man likes a cow that likes a sheep then the man owns a car.').

t(89, 'If there is a man then the man likes a dog that likes a cat and the man likes a cow that likes a sheep.').

t(90, 'If there is a man then the man likes a dog that likes a cat and that likes a rat and the man likes a cow that likes a sheep and that likes a pig.').

t(91, 'If a man is a dog that is a cat and the man is a cow that is a sheep then the man is a car.').

t(92, 'If there is a man then the man is a dog that is a cat and that is a rat and the man is a cow that is a sheep and that is a pig.').

t(93, 'If John likes Mary then Mary likes Bill.').

t(94, 'If John owns a car then there are at least 3 women that like John.').

t(95, 'There is at least 1 man.').

t(96, 'John likes at most 3 women.').
t(97, 'John likes less than 3 women.').
t(98, 'John likes exactly 3 women.').
t(99, 'John likes at least 3 women.').
t(100, 'John likes more than 3 women.').
t(101, 'John likes at most 1 woman.').
t(102, 'John likes less than 1 woman.').
t(103, 'John likes exactly 1 woman.').
t(104, 'John likes at least 1 woman.').
t(105, 'John likes more than 1 woman.').

t(106, 'Everybody who loves somebody loves himself.').

t(107, 'If a man likes Mary and Mary hates a dog then the man owns a car.').

t(108, 'If a man likes Mary and Mary does not hate a dog then the man owns a car.').

t(109, 'Every man is John who owns a car.').

t(110, 'Every man is John who does not own a car.').

t(111, 'Everybody\'s age is 31.').
t(112, 'Everybody\'s address is "Poland".').
t(113, 'If somebody\'s age is 31 then his address is "Poland".').

t(114, 'John\'s father is Bill.').

/* The following 6 sentences cannot be translated into OWL.
Note that 'E' and 'F' are variables and not proper names.
Therefore we have heavy anaphoric references between the IF and the THEN parts.
BUG: Maybe it's possible to convert this to SWRL though?
Probably not, since we have disjunction and negation here. */
t(115, 'If a room contains E and contains F then if the room contains a sculpture X then X is E or is F.').
t(116, 'For every room that contains E and that contains F if the room contains a sculpture X then X is E or is F.').
t(117, 'For every room that contains E and that contains F every sculpture that the room contains is E or is F.').
t(118, 'If a room contains E and contains F then it is false that the room contains a sculpture X and that it is false that X is E or is F.').
t(119, 'If a room contains E and contains F then it is false that the room contains a sculpture X and that X is not E and that X is not F.').
t(120, 'No room that contains E and that contains F contains a sculpture that is not E and that is not F.').

t(121, 'If there is a number X then X + 1 = John.').
t(122, 'If 1.1 * 2 = 2.2 then 0.9 = 2 - 1.1.').
% Note: E is a variable, Pi is a proper name.
t(123, 'If E approaches 2 then 3.14 approaches Pi.').

t(124, 'Bill is John\'s father.').
t(125, 'John\'s father likes Bill.').
t(126, 'Bill likes John\'s father.').
t(127, 'If something X is a father of something Y then X is a parent of Y.').
t(128, 'If something X is a part of something Y and Y is a part of something Z then X is a part of Z.').

% Maps to a SWRL rule with complex classes (negation and disjunction) as atoms
t(129, 'Every man that owns a car and that is not a manager cleans the car.').
t(130, 'Every man that does not ride a car, and that rides a bus or that rides a bike owns a dog that likes the man.').

t(131, 'For every thing X for every thing Y if X owns something that contains Y then X owns Y.').

t(132, 'If a man likes something X then the man sees X.').

t(133, 'John is a man.').

t(134, 'John owns a car.').

t(135, 'John is somebody.').

% BUG: RDF/XML is not generated (note: RDF/XML is deprecated now anyway)
t(136, 'If somebody X sees something that is heard by somebody Y then X likes Y.').

t(137, 'For every day a man does not see a woman.').

t(138, 'For every day John does not see a woman.').

t(139, 'For every day John does not see Mary.').

t(140, 'For every day John sees a dog or sees a cat.').

t(141, 'Exactly 2 countries border Estonia.').

t(142, 'Who does Mary like?').

t(143, 'There is a continent.').
t(144, 'There are at least 4 continents.').
t(145, 'There are exactly 4 continents.').
t(146, 'There are at most 4 continents.').
t(147, 'It is false that there are more than 4 continents.').
t(148, 'It is false that there are exactly 4 continents.').
t(149, 'It is false that there are less than 4 continents.').

t(150, 'Which countries border Estonia?').
t(151, 'Which countries do not border Estonia?').
t(152, 'What is a country that borders Estonia or that Switzerland borders?').

t(153, 'John is fond-of Mary.'). % pos
t(154, 'John is as rich as Mary.'). % pos_as
t(155, 'John is fonder-of Mary.'). % comp
t(156, 'John is more rich than Mary.'). % comp_than
t(157, 'John is fondest-of Mary.'). % sup

/* The following 6 sentences are all logically equivalent.
158--161 are lexically equivalent; 162--163 are lexically equivalent. */
t(158, 'If a room contains Sculpture-E and contains Sculpture-F then if the room contains a sculpture X then X is Sculpture-E or is Sculpture-F.').
t(159, 'For every room that contains Sculpture-E and that contains Sculpture-F if the room contains a sculpture X then X is Sculpture-E or is Sculpture-F.').
t(160, 'For every room that contains Sculpture-E and that contains Sculpture-F every sculpture that the room contains is Sculpture-E or is Sculpture-F.').
t(161, 'If a room contains Sculpture-E and contains Sculpture-F then it is false that the room contains a sculpture X and that it is false that X is Sculpture-E or is Sculpture-F.').
t(162, 'If a room contains Sculpture-E and contains Sculpture-F then it is false that the room contains a sculpture X and that X is not Sculpture-E and that X is not Sculpture-F.').
t(163, 'No room that contains Sculpture-E and that contains Sculpture-F contains a sculpture that is not Sculpture-E and that is not Sculpture-F.').

t(164, 'If Constant-E approaches 2 then 3.14 approaches Constant-Pi.').
t(165, 'If Constant-E approaches 3 then Constant-Pi approaches 4.').

t(166, 'No city overlaps-with a city that is not the city.').
t(167, 'Every city overlaps-with itself or does not overlap-with a city.').
t(168, 'If a city overlaps-with something X then X is the city or X is not a city.').

t(169, 'Every man who owns a car that likes a cat is a dog that is hated by a bike or that likes itself.').
t(170, 'Every man who owns a car is a dog that is hated by a bike or that likes itself.').
t(171, 'Every man is a dog that is hated by a bike or that likes itself.').
t(172, 'Every man is a dog that hates a bike or that likes itself.').

t(173, 'Which countries border a lake and use the lake?'). % should fail
t(174, 'What is a country that borders a territory that guards the country?'). % should fail
t(175, 'What is a country that borders a territory that guards the territory?'). % should succeed

t(176, 'Which territories guard themselves?').
t(177, 'Which territories border more than 3 territories that guard themselves?').
t(178, 'Which territories border nothing but lakes?').
t(179, 'What is a country that borders no sea?').

t(180, 'Everybody\'s ancestor is Adam.').
t(181, 'Everybody\'s ancestor is not Adam.').
t(182, 'Every part of EU is a country.').

t(183, 'If X is a friend of something that is an enemy of Y then X is an enemy of Y.').

t(184, 'Every man owns 2.5 cars.').

t(185, 'If a man owns a car then the man likes the car and likes exactly 3 cats.').
t(186, 'If John owns a car then Mary likes a car and likes at most 3 cats.').
t(187, 'Which man is John?').

t(188, 'John is a man that owns a car.').
t(189, 'John is at least 1 man.').
t(190, 'John is at least 2 men.').
t(191, 'John is a man. Mary is a woman.').
t(192, 'John is a man that is a manager.').
t(193, 'A man is a human.').
t(194, 'A man is John.').
t(195, 'John is John.').
t(196, 'A man is a man.').
t(197, 'A man is the man.').
t(198, 'A country is what?').

t(199, 'John is nothing but Mary.').
t(200, 'John\'s child is not Mary.').
t(201, 'John\'s child is nothing but Mary.').
t(202, 'Every child of John is Mary.').
t(203, 'Whose child is Mary and is nothing but Mary?').
t(204, 'Who is a person whose child is Mary and whose child is nothing but Mary?').

t(205, 'John knows a man. Who is the man?').

t(206, 'John owns a car. Mary owns the car. What is it?').
t(207, 'John owns a car. Mary owns the car. What is it? Bill sees the car.').
t(208, 'John owns a car. Mary owns the car. What is it? Bill sees John.').

t(209, 'Who owns nothing but cats?').

t(210, 'John is a man that sees Mary.').
t(211, 'John is a man that is seen by Mary.').
t(212, 'Mary sees John that is not a man.').

t(213, 'John is a man. Mary is a woman. Everybody likes the woman. Nobody likes the man.').

t(214, 'If there is a circle C and C\'s radius is R and C\'s area is S then S = Pi * (R * R) / 1.').

t(215, 'If John likes Mary then Bill is not William.').

t(216, 'If 1 + 2 = 3 then 1 = 3 - 2.').

t(217, 'If 1 + 2 = X then X = 3.').

t(218, 'If a country X1 surrounds a territory X2 and the territory X2 is a part of a country X3 and it is false that the country X3 is the country X1 then the territory X2 is an enclave.').

t(219, 'If "12" & "34" = "1234" then John owns at most 5 cars.').

t(220, 'If X = 1 + 2 then X = 3.').
t(221, 'For everything John likes Mary.').
t(222, 'If there is X and there is Y then X knows Y.').

% simple adjectives, both attributive and predicative
t(223, 'John is rich.').
t(224, 'John has a fast car.').
t(225, 'John has more than 5 fast cars.').
t(226, 'John has less than 5 fast cars.').
t(227, 'Every fast car is owned by a rich man.').
t(228, 'Every fast car is owned by at least 5 rich men.').
t(229, 'Every fast car is owned by at most 5 rich men.').
t(230, 'If John is rich then Mary is richer.'). % do not support that
t(231, 'If John is rich then Mary is pretty.').
t(232, 'Every talented and beautiful woman is rich and famous.').
t(233, 'Every talented and beautiful woman is rich and famous and is smarter than her own husband.').
t(234, 'Every man has at least 5 cars that are red.').
t(235, 'Every man has at most 5 cars that are red.').

% max. conditions that have a relative clause.
% there was a bug that was fixed only in ape_6_6_101116
t(236, 'Every man knows at least 2 persons whose age is 42.').
t(237, 'Every man knows at most 2 persons whose age is 42.').

t(238, 'Every man knows at least 2 persons whose friend is Bill.').
t(239, 'Every man knows at most 2 persons whose friend is Bill.').

t(240, 'Every man knows at least 2 persons that do not own no cat.').
t(241, 'Every man knows at most 2 persons that do not own no cat.').

% These should map to OWL and not to SWRL (fixed after ape-6.6-101116 was released)
t(242, 'There is X. If X is a fireman then X is a man.').
t(243, 'There is X. There is Y. If X likes Y then X is a man.').
t(244, 'There is X. There is Y. If X likes Y then Y is a man.').
t(245, 'There is X. If X likes John then X is a woman.').

t(246, 'There is X. If John likes X then John is a man.').

t(247, 'If John likes Mary then 1 = 2.').
t(248, 'If John likes Mary then 1 \\= 2.').
t(249, 'If John likes Mary then 1 < 2.').
t(250, 'If John likes Mary then 1 =< 2.').
t(251, 'If John likes Mary then 1 > 2.').
t(252, 'If John likes Mary then 1 >= 2.').

t(253, 'If John likes Mary then 1 is not 2.').
t(254, 'Everybody\'s address is not "Poland".').

t(255, 'There is X. If a man owns a dog that likes X then the dog likes the man.').

t(256, 'Mary\'s brother is John.').
t(257, 'Mary\'s brother is rich and is taller than John\'s father.').
t(258, 'John likes Mary\'s sister.').
t(259, 'Everybody likes something.').
t(260, 'Every man is a brother of somebody.').
t(261, 'Everybody who somebody likes does not hate a dog or is not an alien.').
t(262, 'Every man is taller than 3 women.').

% BUG: does not give an OWL representation (but "John writes every book." does).
t(263, 'John is an author of every book.').

t(264, 'John does not see what?').

% BUG: fails and gives the wrong error message "Yes/no queries not supported".
t(265, 'A man does not see what?').

t(266, 'John does not see a man who sees what?').

% Think about these:
t(267, 'John does not make everything that Mary wants.').
t(268, 'Who does not make everything that Mary wants?').

t(269, 'John is a man or is something and is nothing.').

% Measurement nouns (currently not implemented)
t(270, 'Every man has at least 3kg of rice.').
t(271, 'Every man has at most 3kg of rice.').
t(272, 'John has at least 3kg of rice.').
t(273, 'John has at most 3kg of rice.').
t(274, 'Every man has at least 3kg of apples.').
t(275, 'Every man has at most 3kg of apples.').
t(276, 'John has at least 3kg of apples.').
t(277, 'John has at most 3kg of apples.').

% Unsupported question with 2 query words
t(278, 'What likes what?').
t(279, 'Which man likes which woman?').

% Complex toplevel box
t(280, 'John has more than 2 cats that Mary likes.').
t(281, 'John has at least 2 cats that at least 3 dogs like.').
t(282, 'John knows at least 2 rich men.').
t(283, 'There are more than 3 rich men.').

t(284, 'A man likes Mary that likes the man.').
t(285, 'A man likes a woman that likes the man.').
t(286, 'A man owns at least 2 cars that a woman likes.'). % BUG

main :-
	add_to_lexicon,
	time(test_owlswrl).

test_owlswrl :-
	forall(t(ID, Text), (
		clear_messages,
		acetext_to_drs(Text, _, _, Drs, _Messages),
		drs_to_owlswrl:drs_to_owlswrl(Drs, 'http://attempto.ifi.uzh.ch/ontologies/owlswrl/test', Text, OwlFss),
		get_messages(OwlMessages),
		with_output_to(atom(OwlMessagesPp), maplist(writeln, OwlMessages)),
		format('~w: ~w~n~w~n', [ID, Text, OwlMessagesPp]),
		ignore(owlswrl_to_fss(OwlFss)), nl,
		output_xml(OwlFss)
		)
	).


output_xml(Owlswrl) :-
	catch(
		(
			owlswrl_to_xml(Owlswrl, OwlXml),
			xmlterm_to_xmlatom(OwlXml, OwlXmlAtom),
			%store_as_file(ID, OwlXmlAtom),
			write(OwlXmlAtom), nl, nl
		),
		CatchType,
		(
			writeq(CatchType), nl
		)
	).


store_as_file(ID, Atom) :-
	concat_atom(['testruns/owlswrl/t', ID, '.owl'], Path),
	tell(Path),
	write(Atom),
	told.


% A hackish way to override some entries in the lexicon
add_to_lexicon :-
	asserta(clex:noun_sg(apple, 'iri|http://www.example.org/words#apple', neutr)),
	asserta(clex:pn_sg('Bill', iri('http://www.example.org/words#Bill'), neutr)).
