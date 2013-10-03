echo Compiling APE...
cd ../parser
bash compile.sh
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

zip -r \
    ape-6.7-$timestamp.zip \
        ape.pl \
        CHANGES.md \
        get_ape_results.pl \
        LICENSE.txt \
        make_exe.bat \
        make_exe.sh \
        README.md \
        runape.pl \
        run.sh \
        acetext_drs.pl \
        examples/ \
        java/install-jpl.sh \
        java/pom.xml \
        java/README.md \
        java/src/ \
        lexicon/ \
        logger/ \
        parser/ \
        utils/ \
    -x \
        \*.tmp \*.gitignore \*.plp

mv lexicon/clex_lexicon.pl lexicon/clex_lexicon_small.pl
mv lexicon/clex_lexicon.pl.tmp lexicon/clex_lexicon.pl
rm acetext_drs.pl

echo Finished.
