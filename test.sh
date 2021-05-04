set -o nounset

mydir=$(dirname "$0")
test_dir="test/"


swipl=$(which swipl 2> /dev/null) ||{
	echo "swipl is not installed, --exiting" >&2
	exit
}

for test_file in $(find "$mydir/$test_dir" -name "*.pl"); do
   echo -n "Running '$test_file'" >&2
   "$swipl" -g run_tests -t halt "$test_file"
done
