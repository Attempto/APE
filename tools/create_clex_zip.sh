rm ../clex-*.zip

mkdir clex
mv ../lexicon/clex_lexicon.pl clex/
csplit ../LICENSE.txt '%                    GNU GENERAL PUBLIC LICENSE%'
mv xx00 clex/LICENSE.txt

timestamp=`date '+%y%m%d'`
zip ../clex-6.6-$timestamp.zip clex/clex_lexicon.pl clex/LICENSE.txt

mv clex/clex_lexicon.pl ../lexicon/
rm -r clex
