echo Compiling APE...
cd ../parser
bash compile.sh
cd ..

echo Building the Attempto Java Packages...
cd java
ant
cd ..

mv lexicon/clex_lexicon.pl lexicon/clex_lexicon.pl.tmp
mv lexicon/clex_lexicon_small.pl lexicon/clex_lexicon.pl

echo "Generating public regression test set..."
cd tests
swipl -f make_acetext_drs.pl -g main -t halt -q > ../acetext_drs.pl
cd ..

echo Creating ZIP file...
rm ape-*.zip

timestamp=`date '+%y%m%d'`

zip \
    ape-6.6-$timestamp.zip \
        * \
        lexicon/* \
        logger/* \
        parser/* \
        utils/* \
        utils/owlswrl/* \
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

mv lexicon/clex_lexicon.pl lexicon/clex_lexicon_small.pl
mv lexicon/clex_lexicon.pl.tmp lexicon/clex_lexicon.pl
rm acetext_drs.pl

echo Finished.
