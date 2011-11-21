
ape=../ape.exe

# This test should print out 9 times:
#
# If there is a user X1 then Attempto_Parsing_Engine helps the user X1.
#

file_file1=../../web/site/acetexts/example1.ace.txt
file_url1=http://attempto.ifi.uzh.ch/site/acetexts/example1.ace.txt
ulexfile_file1=../../web/site/acetexts/example1.ulex.pl
ulexfile_url1=http://attempto.ifi.uzh.ch/site/acetexts/example1.ulex.pl

$ape -text "APE helps every user." -ulextext "pn_sg('APE', 'Attempto_Parsing_Engine', neutr). pn_pl('APEs', 'Attempto_Parsing_Engines', neutr)." -solo paraphrase1
$ape -text "APE helps every user." -ulexfile $ulexfile_file1 -solo paraphrase1
$ape -text "APE helps every user." -ulexfile $ulexfile_url1 -solo paraphrase1

$ape -file $file_file1 -ulextext "pn_sg('APE', 'Attempto_Parsing_Engine', neutr). pn_pl('APEs', 'Attempto_Parsing_Engines', neutr)." -solo paraphrase1
$ape -file $file_file1 -ulexfile $ulexfile_file1 -solo paraphrase1
$ape -file $file_file1 -ulexfile $ulexfile_url1 -solo paraphrase1

$ape -file $file_url1 -ulextext "pn_sg('APE', 'Attempto_Parsing_Engine', neutr). pn_pl('APEs', 'Attempto_Parsing_Engines', neutr)." -solo paraphrase1
$ape -file $file_url1 -ulexfile $ulexfile_file1 -solo paraphrase1
$ape -file $file_url1 -ulexfile $ulexfile_url1 -solo paraphrase1


# This test should preserve the Unicode characters.

file_file2=../../web/site/acetexts/example2.ace.txt

$ape -file $file_file2 -solo paraphrase1
