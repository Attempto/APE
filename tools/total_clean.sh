#!/bin/sh

echo "About to delete files:"

find . -name "*.plp"
find . -name "*.exe"
find . -name ".DS_Store"

echo "deleting..."

find . -name ".DS_Store" | xargs rm
find . -name "*.plp" | xargs rm
find . -name "*.exe" | xargs rm

echo "done."
