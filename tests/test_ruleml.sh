#==============================================================================
# This script validates the RuleML output of APE.
#
# Author: Tobias Kuhn
#
# The programs 'xmllint' and 'trang' need to be installed.
#
# THIS SCRIPT CURRENTLY DOES NOT WORK:
# I tried different XML validation tools like xmllint and xerces and different
# schema files, but I failed to validate even the official RuleML examples.
#==============================================================================


# Location of APE executable:
ape=../ape.exe

# Temporary directory:
tempdir=tmp/ruleml

echo "=== Create clean temporary directory: $tempdir ==="
mkdir -p $tempdir/output
mkdir -p $tempdir/schema

echo "=== Create RuleML output for test sentences ==="
$ape -text "Every customer is important." -solo ruleml > $tempdir/output/1.xml
$ape -text "John is a man." -solo ruleml > $tempdir/output/2.xml
$ape -text "Nobody waits." -solo ruleml > $tempdir/output/3.xml

echo "=== Get XML schema ==="
if [ -e $tempdir/schema/folog_normal.rng ]
then
  echo "Use cached schema file: $tempdir/schema/folog_normal.rng"
else
  echo "Download and convert schema file..."
  trang http://ruleml.org/1.0/relaxng/folog_normal.rnc $tempdir/schema/folog_normal.rng
fi

echo "=== Validate RuleML output with schema ==="
xmllint --noout --relaxng $tempdir/schema/folog_normal.rng $tempdir/output/1.xml
xmllint --noout --relaxng $tempdir/schema/folog_normal.rng $tempdir/output/2.xml
xmllint --noout --relaxng $tempdir/schema/folog_normal.rng $tempdir/output/3.xml

echo "=== Finished ==="
