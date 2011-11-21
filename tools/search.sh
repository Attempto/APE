# Looks for a certain sequence of characters (<searchword>) from all the
# pl-files in the current directory and its subdirectories.
# @author Kaarel Kaljurand
# @version 2008-04-16

if [ $# -eq 1 ]
then
	find . -name "*.pl" | xargs grep $1
else
	echo "Usage: search.sh <searchword>"
fi
