Attempto Java Packages
======================

Copyright 2008-2011, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).

The Attempto Java Packages can be downloaded from http://attempto.ifi.uzh.ch/site/downloads/.

The Attempto Java Packages are a collection of Java packages that are related to the Attempto
project. The Attempto Java Packages are free software licensed under the GNU Lesser General Public
Licence (see licenses/gpl-3.txt and http://www.gnu.org/licenses/lgpl.html).

See lib/README.txt for information about the used third-party libraries.


1. Content
----------

The Attempto Java Packages consists of one jar-file:

- attempto-ape.jar contains interfaces to the Attempto Parsing Engine (APE).

Note that AceWiki and the ACE Editor are no longer part of the Attempto Java Packages, but are now
hosted on Launchpad:

  https://launchpad.net/acewiki

See docs/index.html for the detailed documentation of the packages and classes.


2. Compilation
--------------

This package is pre-compiled. Thus, compilation is needed only if the source code is changed. The
compilation can be run manually using Ant. Make sure that a recent version of Apache Ant is
installed. The following commands are available:

"ant compile" compiles the Java source code.

"ant createjars" creates the jar-files.

"ant createjavadoc" creates the Javadoc documentation pages.

"ant clean" deletes all automatically generated files like the compiled Java classes, the
jar-files, and the Javadoc files.

"ant buildeverything" builds everything from scratch.


3. APELocal
-----------

The APELocal class is an interface to the ACE parser that is written in SWI Prolog. When this class
is used directly or indirectly, you have to make sure that a recent version of SWI Prolog is
installed and that the file "ape.exe" is available. The file "ape.exe" can be complied from the APE
package that is available on the Attempto download page.

Furthermore, you have to use the following Java VM argument that points to the location where the
SWI Prolog system file libjpl.jnilib (under Mac OS X), jpl.dll (under Windows), or libjpl.so (under
Unix) is located:

  -Djava.library.path="/opt/local/lib/swipl-5.6.45/lib/i386-darwin8.10.1"

Note that the exact path is most probably different on your system.


5. Mailing List
---------------

If you encounter problems, you can get help from the Attempto community. Visit the Attempto Mailing
List site:

  http://attempto.ifi.uzh.ch/site/mailinglist/
