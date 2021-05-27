#!/bin/bash

set -o nounset

mydir=$(dirname "$0")
ex_dir="examples/"
swipl=$(which swipl 2> /dev/null) ||{
	echo "swipl is not installed, --exiting" >&2
	exit
}

for ex_file in $(find "$mydir/$ex_dir" -name "*.hofl"); do
   echo -e "Running '$ex_file'\n" >&2  
   lat=$(sed 's/hofl/tex/g' <(echo "$ex_file"))
   "$swipl" "$mydir/hofltex.pl" -o "$lat" -f "$ex_file"  
done
