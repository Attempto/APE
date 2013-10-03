#
# Packages the main contents of the repository as a zip-file.
# Includes the jar-file and Javadoc for the Java interface for APE.
#
# Note: For a new release you probably need to update the version number
# in 'ape-6.7'.
#
echo Compiling APE...
cd ../parser
bash compile.sh
cd ..

echo Building the Java Interface for APE...
cd java
mvn package site -DskipTests
cd ..

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
        java/target/*.jar \
        java/target/site \
        lexicon/ \
        logger/ \
        parser/ \
        utils/ \
    -x \
        \*.gitignore \*.plp

rm acetext_drs.pl

echo Finished.
