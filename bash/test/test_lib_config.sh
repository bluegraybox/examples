#!/bin/bash

pass_count=0
fail_count=0
function assert_equal () {
    if [[ "$1" != "$2" ]] ; then
        echo "Expected '$2', got '$1' "
        fail_count=$((fail_count + 1))
    else
        echo -n ". "
        pass_count=$((pass_count + 1))
    fi
}

this=$(readlink -f $0)
abs_dir=$(dirname $this)
script=$(basename $0)
rel_dir=$(dirname $0)

source $abs_dir/../lib_config.sh

### Test init_config
init_config "$abs_dir/test_lib_config.sh"
assert_equal $_LCX_dir "$abs_dir"
assert_equal $_LCX_config "$abs_dir/.test_lib_config.cfg"
assert_equal $_LCX_log "$abs_dir/test_lib_config.log"

init_config "$rel_dir/test_lib_config.sh"
assert_equal $_LCX_dir "$abs_dir"
assert_equal $_LCX_config "$abs_dir/.test_lib_config.cfg"
assert_equal $_LCX_log "$abs_dir/test_lib_config.log"

### Test save_config and load_config
a="apple"
b="bumble bee"
save_config a b

# Test different values
a=foo
b=bar
load_config
assert_equal "$a" "apple"
assert_equal "$b" "bumble bee"

# Test blank values
unset a b
load_config
assert_equal "$a" "apple"
assert_equal "$b" "bumble bee"

# Test comment persistence
echo "# intro comment" > $_LCX_config
save_config a b
echo "# later comment" >> $_LCX_config
c="cow"
d="dog"
save_config c d
unset a b c d
assert_equal "$(grep intro $_LCX_config)" "# intro comment"
assert_equal "$(grep later $_LCX_config)" "# later comment"
load_config
assert_equal "$a" "apple"
assert_equal "$b" "bumble bee"
assert_equal "$c" "cow"
assert_equal "$d" "dog"

### Test confirm_config
# echo "Enter 'foo' for 'a' and 'bar baz' for 'b'"
# confirm_config a b
# assert_equal "$a" "foo"
# assert_equal "$b" "bar baz"
unset a b

### Test process_options
process_options "[d]=debug [v]=verbose" "[n]=name [a]=age"
assert_equal "$debug" ""
assert_equal "$verbose" ""
assert_equal "$name" ""
assert_equal "$age" ""
unset debug verbose name age

process_options "[d]=debug [v]=verbose" "[n]=name [a]=age" -d
assert_equal "$debug" 1
assert_equal "$verbose" ""
unset debug verbose name age

process_options "[d]=debug [v]=verbose" "[n]=name [a]=age" -d -n "foo bar" -d
assert_equal "$debug" 2
assert_equal "$verbose" ""
assert_equal "$name" "foo bar"
assert_equal "$age" ""
unset debug verbose name age

process_options "[d]=debug [v]=verbose" "[n]=name [a]=age" -a 20 -d -n "foo bar" -v -d
assert_equal "$debug" 2
assert_equal "$verbose" 1
assert_equal "$name" "foo bar"
assert_equal "$age" "20"
unset debug verbose name age


echo
echo "$pass_count passed"
echo "$fail_count failed"

