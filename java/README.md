Java Interface for APE
======================

The Java Interface for APE is part of the Attempto Parsing Engine (APE).
Copyright 2008-2013, Attempto Group, University of Zurich (http://attempto.ifi.uzh.ch).


Content
-------

To simplify calling APE from Java programs, we provide the `ACEParser` interface which
is effectively a Java-style front-end to querying the predicate `get_ape_results/2`.
`ACEParser` is implemented by the following classes:

  - `APELocal` accesses APE via JPL, the bidirectional Prolog/Java interface, which is included in the SWI-Prolog installation;
  - `APESocket` accesses APE via its socket server;
  - `APEWebservice` accesses APE via its HTTP server.


Building with Maven
-------------------

The Java Interface for APE can be compiled into a single jar-file `attempto-ape.jar`.

First install SWI-Prolog's JPL into your local Maven repository.
See [install-jpl.sh](install-jpl.sh) for an example on how do it.

Building the jar-file.

	mvn package -DskipTests

Building the documentation, licenses, etc.

	mvn site


Testing
-------

In order to run the unit tests (`mvn test`), first compile `ape.exe`.

For the `APELocal` tests to succeed, place `ape.exe` into the APE root directory
and execute (in any directory):

	eval `swipl -dump-runtime-variables`
	export LD_PRELOAD=$PLBASE/lib/$PLARCH/libjpl.so:$PLBASE/lib/$PLARCH/libswipl.so

For the `APESocket` tests to succeed, start the APE socket server:

	ape.exe -server -port 5000

For the `APEWebservice` tests to succeed, start the APE HTTP server:

	ape.exe -httpserver -port 8000

Now run

	mvn test
