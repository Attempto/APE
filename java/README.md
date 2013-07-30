Java Interface for APE
======================

Copyright 2008-2012, Attempto Group, University of Zurich (http://attempto.ifi.uzh.ch).

The Java Interface for APE is part of the Attempto Parsing Engine
(https://github.com/Attempto/APE).

The Java Interface for APE is free software licensed under the GNU Lesser General Public
Licence (see LICENSE.txt and http://www.gnu.org/licenses/lgpl.html).

See java/lib/README.txt for information about the used third-party libraries.


Content
-------

The Java Interface for APE consists of one jar-file `attempto-ape.jar`, which contains interfaces
to the Attempto Parsing Engine (APE).

See docs/index.html for the detailed documentation of the packages and classes (you might have to
run the respective Ant command first, see below).


Compilation
-----------

Apache Ant is needed for compilation. The following commands are available:

- `ant compile` compiles the Java source code.
- `ant createjars` creates the jar-files.
- `ant createjavadoc` creates the Javadoc documentation pages.
- `ant clean` deletes all automatically generated files like the compiled Java classes, the
  jar-files, and the Javadoc files.
- `ant buildeverything` builds everything from scratch.


APELocal
--------

The APELocal class is an interface to the ACE parser that is written in SWI Prolog. When this class
is used directly or indirectly, you have to make sure that a recent version of SWI Prolog is
installed and that the file ape.exe is available.
