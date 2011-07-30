#!/bin/bash

# All variables used internally by this library are prefixed with _LCX_.


# Make sure that the variables we're going to set aren't set already.
function _LCX_check_unset () {
    for x in "$@" ; do
        [ -n "${!x}" ] && echo "$x should not be set!" && exit 1
    done
}
_LCX_check_unset _LCX_dir _LCX_config _LCX_log


function init_config () {
    local canonical=$(readlink -e -v $1)
    [ -z $canonical ] && echo "No canonical path for $1" && exit 1
    _LCX_dir=$(dirname $canonical)
    local script=$(basename $canonical)
    # strip off the suffix
    local base=${script%.*}
    # in case script starts with a period and doesn't have a suffix
    [ -z $base ] && base=$script
    _LCX_config=$_LCX_dir/.${base#.}.cfg
    _LCX_log=$_LCX_dir/${base#.}.log
}


function load_config () {
    [ -f $_LCX_config ] && source $_LCX_config
}


function confirm_config () {
    for x in $* ; do
        old_x="${!x}"
        # echo -n "What is $x? [${!x}] "
        read -p "What is $x? [${!x}] " new_x
        test -n "$new_x" && declare $x="$new_x" && echo "$x changed to '${!x}'"
    done
}


function save_config () {
    for x in $* ; do
        # if it's in the config, update it.
        if grep -q "^$x=" $_LCX_config ; then
            sed -i "s/^$x=.*/$x=\"${!x}\"/;" $_LCX_config
        else
            echo "$x=\"${!x}\"" >> $_LCX_config
        fi
    done
}


function process_options () {
    # $1 and $2 are strings that evaluate to associative arrays.
    # For example, "([f]=foo [b]=bar)"
    # They match options to variables, so the option "-f baz" causes the variable foo to be set to "baz".
    # Flag options don't take a parameter; we just count how often they appear.
    # So if the above example were the flags option, "-f -f -f" would cause the variable foo to be set to 3.
    local -A flags=$1 values=$2
    shift 2
    local flag_opt="" value_opt="" x
    for x in "${!flags[@]}" ; do flag_opt="$flag_opt$x" ; done
    for x in "${!values[@]}" ; do value_opt="$value_opt$x:" ; done

    while getopts ":$flag_opt$value_opt" option "$@" ; do
        case $option in
            \?) echo "Unknown option -$OPTARG" ;;
            [$flag_opt])
                x=${flags[$option]}
                # 'export' makes these visible outside this function; otherwise, use 'local'
                export $x=${!x:-0}
                export $x=$((${!x} + 1)) ;;
            [$value_opt]) export ${values[$option]}="$OPTARG" ;;
        esac
    done
    return $OPTIND
}

