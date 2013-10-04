APE Change Log
==============

Version 6.7-131003
------------------

This release implements two new ACE features and brings minor improvements.

- New in ACE 6.7: Support for the exponentiation operator ^ in arithmetical expressions,
  e.g. "2^3^4", which is parsed as (2^3)^4.
- New in ACE 6.7: Support for how many/how much queries, which ask for numbers of
  countable nouns or for amounts of mass or measurement nouns,
  e.g. "How many beaches have how much sand?"
- APE can now translate ACE texts into the RuleML format.
- The coverage of the ACE-to-OWL translator has been improved.
- The dependencies of the Java Interface for APE on external libraries are now managed using Maven.
- The large Clex lexicon (~100,000 entries) was migrated to a separate repository [Attempto/Clex](https://github.com/Attempto/Clex).
  As before, the APE source distribution contains a reduced (~2000 entries) version of this lexicon.


Version 6.6-110816
------------------

This is a bugfix release that brings minor changes and updates:

- ACE to DRS translation:
  - fixed: sentences like "For every woman of who everybody waits." now correctly fail
  - fixed: a bug that in rare cases generated a DRS with duplicate referents
  - fixed: duplicate DRS conditions are now always suppressed
  - fixed: misguiding error message for imperatives
  - deprecated: the construct "the ... of which/who"
- DRS to FOL translation:
  - fixed: disjunctions related by anaphoric references (e.g. the DRS of "A man sleeps or the man
    waits.") are now correctly mapped to FOL
- DRS to ACE translation:
  - improved: handling of disjuntions, e.g. "Every dog barks or a cat sleeps." is now paraphrased
    as "A cat sleeps or if there is a dog X1 then the dog X1 barks."
  - restored: support for 0, e.g. "There are 0 men."
- DRS to OWL/SWRL translation:
  - the pretty-printed output (owlfsspp) now conforms to the OWL Functional-Style Syntax (and can
    be parsed by the OWL-API)
  - avoid including redundant owl:Thing in intersections
  - now supported: the DRS of "John does not see what?"
  - fixed: sentences like "There is X. If X is a fireman then X is a man." now map to OWL (instead
    of SWRL as before)
  - improved: the SWRL-mapping now supports formula/3 with =<, >=, \=
  - fixed: "Peter owns 3 kg of apples." now correctly fails
- Attempto Java Packages:
  - updated the Apache HTTP libraries to httpcore-4.1.1.jar and httpclient-4.1.1.jar
  - removed the OWL Verbalizer Webservice interface, this is now available at
    owlverbalizer.googlecode.com
  - updated enum OutputType: removed OWLRDF, added TPTP
- The APE regression test set (~3500 small ACE texts with their corresponding DRSs) is now included
  (see ape/acetext_drs.pl)
- Several improvements to the documentation of ACE, DRS and the APE source code.
