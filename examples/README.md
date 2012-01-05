Examples
========

Introduction
------------

Some examples of using ACE->DRS, DRS->ACE, and ACE->OWL programmatically.


Usage
-----

Examples of calling the scripts from the command line:

    $ swipl -s ace_to_ace.pl -g "t('Every man is a human.')" -t halt
    
    $ swipl -s ace_to_owl.pl -g "t('Every man is a human.')" -t halt
    
    $ swipl -s paraphrase_roundtrip.pl -g paraphrase_roundtrip -t halt
