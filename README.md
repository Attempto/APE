APE - ACE Parsing Engine
========================

Authors: Kaarel Kaljurand, Norbert E. Fuchs, Tobias Kuhn


Introduction
------------

This document explains how APE (ACE Parsing Engine) is compiled and used.

In order to compile and run APE, you first need to install a recent version of SWI-Prolog.
SWI-Prolog is free software and can be downloaded from <http://www.swi-prolog.org>. Note that you
minimally need to install the following SWI Prolog packages: `clib`, `sgml`, and `http`. To view
the documentation embedded in the source files you also need `pldoc`.


Compilation
-----------

Before you can run APE, you have to compile the APE source code. Just execute the file
`make_exe.bat` in the case of Windows or `make install` in the case of Mac OS X, Linux, or any other
Unix system. Both the bat-file and the Makefile are located in the root directory of the APE distribution. As a result (and
given that there were no compilation errors), a new file `ape.exe` is created in the current
directory.

(In some unlikely cases you might have to change the size of the memory areas
used by SWI-Prolog. This is documented at <http://www.swi-prolog.org/pldoc/man?section=memlimit>.)


Execution
---------

APE has to be executed from the command-line. In the command-line terminal, go to the root
directory of APE (where `ape.exe` is located). Then type `ape.exe` in the case of Windows or
`./ape.exe` otherwise. As a result, you should see the following output:

    Attempto Parsing Engine for ACE 6.7, version 6.7-131003
    Copyright 2008-2013, Attempto Group, University of Zurich
    This program comes with ABSOLUTELY NO WARRANTY.
    This is free software, and you are welcome to redistribute it under certain conditions.
    Please visit http://attempto.ifi.uzh.ch for details.
    
    Command-line arguments:
    -text "TEXT"        The input ACE text. If neither -text nor -file is present then the ACE text
                        is read from stdin.
    -file FILENAME      The name or URL of the input file containing the ACE text.
    -ulextext "TEXT"    The user lexicon (taken from a string).
    -ulexfile FILENAME  The user lexicon (taken from a file or URL).
    -solo OUTPUT        Output just one output component. OUTPUT has to be one of {paraphrase,
                        paraphrase1,paraphrase2,owlfss,owlfsspp,owlrdf,owlxml,ruleml,fol,pnf,tptp,
                        tokens,syntax,syntaxpp,syntaxd,syntaxdpp,drs,drsxml,drspp,drshtml}.
    -cdrs               Output the DRS as a Prolog term.
    -cdrsxml            Output the DRS in XML.
    -cdrspp             Output the DRS in pretty-printed form in plain text.
    -cdrshtml           Output the DRS in pretty-printed form in HTML.
    -cparaphrase        Output a paraphrase which is a "best-effort" combination of paraphrase1 and
                        paraphrase2.
    -cparaphrase1       Output a paraphrase which uses full sentences instead of relative clauses.
    -cparaphrase2       Output a paraphrase which uses relative clauses instead of full sentences.
    -ctokens            Output tokens as a Prolog list of lists.
    -csentences         Output sentences as a Prolog list.
    -csyntax            Output simplified syntax trees as a Prolog list.
    -csyntaxpp          Output simplified syntax trees in pretty-printed form.
    -csyntaxd           Output plain syntax trees as a Prolog list (for debugging).
    -csyntaxdpp         Output plain syntax trees in pretty-printed form (for debugging).
    -cowlfss            Output OWL/SWRL in the Functional-Style Syntax representation (as Prolog
                        term).
    -cowlfsspp          Output OWL/SWRL in the Functional-Style Syntax representation (pretty-
                        printed).
    -cowlxml            Output OWL/SWRL in the XML representation.
    -cowlrdf            Output OWL/SWRL in the RDF/XML representation. DEPRECATED
    -cruleml            Output RuleML representation of the DRS.
    -cfol               Output standard first-order logic representations (default form) of the DRS
                        as a Prolog term.
    -cpnf               Output standard first-order logic representations (prenex normal form) of
                        the DRS as a Prolog term.
    -ctptp              Output TPTP representation of the DRS.
    -uri URI            URI for the OWL outputs.
    -noclex             Ignore the lexicon entries that are compiled into the executable.
    -guess              Guess the word-class of unknown words.
    -server             Launch a socket interface to APE at port 2766 (0xACE).
    -httpserver         Launch an HTTP interface to APE at port 8000.
    -port NUMBER        Override the default port of either the socket or the HTTP interface.
    -version            Shows version information.
    -help               Shows this help page.

APE can be used via four different interfaces:

- command-line interface
- socket interface
- HTTP interface (webservice)
- directly from Prolog or Java

All these possibilities are described in the following sections.


### Command-line interface to APE

The following command parses the text "John waits." and outputs the DRS in XML representation and
the syntax tree:

    ./ape.exe -text "John waits." -cdrsxml -csyntax

In the case of Windows, you have to omit the first two characters `./`. The next example parses the
text that is inside of the file `ace.txt` and outputs the OWL FSS representation:

    ./ape.exe -file ace.txt -solo owlfss

If you omit both arguments, `text` and `file`, the ACE text is read from the standard input:

    echo "Every mammal is an animal." | ./ape.exe -solo drspp

Note that this does not work under Windows.

If you just execute the line

    ./ape.exe -solo drspp

then the terminal waits for an input. In this case, you can type your ACE text into the terminal
window. Once you have done so, press `Enter` and `Ctrl-D` to tell the terminal that you are
finished. The output (the pretty printed DRS in our case) is then shown below the ACE text you just
entered. Again, this does not work under Windows.


### Socket interface to APE

A socket interface to APE is started by giving the argument `-server` to APE. The default port,
2766 (0xACE), can be overridden by specifying a different port number as a value to the argument
`-port`.

A good way to start the server on a Unix command-line (e.g. _bash_) is:

    nohup swipl -x ape.exe -- -server -port 3453 > stdout.txt 2> stderr.txt &

On Mac OS X, one could use _launchctl_ instead.

The socket interface knows only one command:

    get(Parameters).

where `Parameters` is a list of parameters accepted by `get_ape_results/2`. Note the dot at the end
of the command! (There should also be a newline after the dot.) Given the input, the server runs
`get_ape_results/2` on it and sends back the results, followed by
`APESERVERSTREAMEND` on a separate line. If the input command is syntactically incorrect then an
error message is logged into STDERR. In any case, the connection to the client is closed.

Examples of input commands:

    get([text='Every man is a human.', cparaphrase1=on]).
    get([text='Every man is a human.', solo=paraphrase1]).
    get([text='Every man is a a human.', cparaphrase1=on]).
    get([text='Every man is a human.', cinput=on, cdrs=on, cparaphrase=on, ctokens=on, csyntax=on]).

Session example, assuming that the APE socket server listens at port 3453:

    $ telnet 127.0.0.1 3453
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    get([text='Every man is a human.', solo=paraphrase1]).
    If there is a man X1 then the man X1 is a human.
    APESERVERSTREAMEND
    Connection closed by foreign host.


### HTTP interface to APE

An HTTP interface to APE is started by giving the argument `-httpserver` to APE. The default port,
8000, can be overridden by specifying a different port number as a value to the argument `-port`.

A good way to start the server on a Unix command-line (e.g. _bash_) is:

    nohup swipl -x ape.exe -- -httpserver -port 8001 > stdout.txt 2> stderr.txt &

On Mac OS X, one could use _launchctl_ instead.

In case there is an error message in the error output, saying that

    ERROR: socket_error(Address already in use)

then try another port number. For example, one would get this error when running the above command
twice: during the first time a server is started which starts to listen requests at port 8001,
during the second time a second server is started but since the port is already being used by the
first server, there is a conflict and the second server crashes.

You can test the server by loading the following URL in your browser.

    http://localhost:8001/?text=Every+man+is+a+human.&solo=drshtml

The result should be an HTML-rendering of the DRS of the sentence "Every man is a human.".

The complete description of this webservice interface as well as some example clients are available
at http://attempto.ifi.uzh.ch/site/docs/ape_webservice.html. (Note though that the parameter
`ulexreload` is not supported by this HTTP interface.)

Note that the parameter names and values, and their meaning is the same as for the command-line
client. Also the results' format is the same. The only difference is that for security reasons the
webservice cannot access local files, i.e. the input ACE text can be passed as a string or via a
pointer an HTTP resource (i.e. URL) but not via pointing to a local file (such as
`/var/acetexts/sometext.ace.txt`).


### Using APE from Java programs

See the documentation in [java/](java/).


### Some examples

ACE text from URL

    ./ape.exe -file http://attempto.ifi.uzh.ch/site/acetexts/example1.ace.txt -solo owlxml

Lexicon from string

    ./ape.exe -text "Every mman is a hhuman." -ulextext "noun_sg(mman, mman, masc). noun_sg(hhuman, hhuman, neutr)." -cparaphrase1

Lexicon from a file

    echo "noun_sg(mman, mman, masc). noun_sg(hhuman, hhuman, neutr)." > ulex.pl
    ./ape.exe -text "Every mman is a hhuman." -ulexfile ulex.pl -cparaphrase1

Reading from STDIN and writing to STDOUT can be used in order to chain several executions of APE
together. The following example paraphrases the paraphrase of "Every man is a human.".

    ./ape.exe -text "Every man is a human." -solo paraphrase1 | ./ape.exe -solo paraphrase2

The following commands have the same meaning.

    ./ape.exe -httpserver
    swipl -x ape.exe -- -httpserver
    swipl -x ape.exe -g http_server

The only difference is in the the way SWI Prolog is called, either via the full path name embedded
in `ape.exe` or via the name `swipl` which the command-line environment must resolve to the full
path name.

By overriding the goal (with `-g`), it is also possible to execute SWI-Prolog commands in the
context of `ape.exe`. For example, the following command displays the source code of `ape.exe`.

    swipl -x ./ape.exe -g listing


Code
----

The distribution includes the following packages containing the main code in Prolog
(in the prolog-directory):

- `logger/`  contains the error logger module
- `lexicon/` contains various lexicon files, notably a content words lexicon with ~2,000 entries
- `utils/`   contains various modules, mostly for translating the Discourse Representation
             Structure (DRS) generated by APE into other logical forms
- `parser/`  contains the Attempto Parsing Engine (APE) (tokenizer, grammar files, anaphoric
             reference resolver)

This listing reflects the dependencies: `logger` depends on no other package. `lexicon` only
depends on `logger`. The package `utils` only depends on `logger` and `lexicon`. The package
`parser`, finally, depends on all three other packages. The files in the root directory depend on
those packages, but not vice versa.

Provided that you have PlDoc installed (SWI-Prolog package `pldoc`), you can view the documentation
by:

    ?- doc_server(1234), [get_ape_results], doc_browser.

Apart from these main packages, there are some additional folders:

- `examples/` contains some Prolog examples of how to programmatically access APE
- `java/`     contains the code of the Java binding for APE
- `tests/`    contains various tests
- `tools/`    contains some auxiliary scripts

For more information consult the commented source files.


Mailing List
------------

If you encounter problems, you can get help from the Attempto community. Visit the Attempto Mailing
List site:

  http://attempto.ifi.uzh.ch/site/mailinglist/
