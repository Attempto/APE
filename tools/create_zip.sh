echo Compiling APE...
cd ../parser
bash compile.sh
cd ../..

echo Building the Attempto Java Packages...
cd java
ant
cd ..

mv ape/lexicon/clex_lexicon.pl ape/lexicon/clex_lexicon.pl.tmp
mv ape/lexicon/clex_lexicon_small.pl ape/lexicon/clex_lexicon.pl

echo "Generating public regression test set..."
cd ape/tests/
swipl -f make_acetext_drs.pl -g main -t halt -q > ../acetext_drs.pl
cd ../../

echo Creating ZIP file...
rm ape/ape-*.zip

timestamp=`date '+%y%m%d'`

zip \
    ape/ape-6.6-$timestamp.zip \
        ape/* \
        ape/lexicon/* \
        ape/logger/* \
        ape/parser/* \
        ape/utils/* \
        ape/utils/owlswrl/* \
        java/* \
        java/src/* \
        java/src/ch/uzh/ifi/attempto/* \
        java/src/ch/uzh/ifi/attempto/ape/* \
        java/docs/* \
        java/docs/ch/uzh/ifi/attempto/ape/* \
        java/docs/src-html/ch/uzh/ifi/attempto/ape/* \
        java/docs/resources/* \
        java/lib/* \
        java/licenses/* \
    -i \
        *.pl *.fit *.txt *.perl *.html *.xml *.java *.jar *.css \
        */package-list */inherit.gif ape/make_exe.*

mv ape/lexicon/clex_lexicon.pl ape/lexicon/clex_lexicon_small.pl
mv ape/lexicon/clex_lexicon.pl.tmp ape/lexicon/clex_lexicon.pl
rm ape/acetext_drs.pl

echo Finished.
