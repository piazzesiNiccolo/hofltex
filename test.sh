set -o nounset

mydir=$(dirname "$0")

swipl=$(which swipl 2> /dev/null) ||{
	echo "swipl is not installed, --exiting" >&2
	exit
}
test_files=(
 test/hofl_test.pl
 test/tokenize_test.pl
 test/type_test.pl
)


for test_file in "${test_files[@]}"; do
   test_file_q="$mydir/$test_file"
   if [[ ! -f "$test_file_q" ]]; then
      echo "Skipping '$test_file' because there is no such file" >&2
      continue
   fi
   echo -n "Running '$test_file'" >&2
   "$swipl" -g run_tests -t halt "$test_file_q"
done
