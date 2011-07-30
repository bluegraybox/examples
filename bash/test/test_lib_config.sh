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


echo
echo "$pass_count passed"
echo "$fail_count failed"

