#!/bin/bash

# invoke this script with:
#     source /path/to/lib_config.sh $0


# All variables used internally by this library are prefixed with _LCX_.


# Make sure that the variables we're going to set aren't set already.
function _LCX_check_unset () {
    for x in "$@" ; do
        [ -n "${!x}" ] && echo "$x should not be set!" && exit 1
    done
}
_LCX_check_unset _LCX_dir _LCX_config _LCX_log


function _init_config () {
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
_init_config "$1"


function load_config () {
    [ -f $_LCX_config ] && source $_LCX_config
}


function confirm_config () {
    # $1 is a string that evaluates to an associative array.
    # For example, "([foo]=\"your foo\" [b]=\"your bar\")"
    # They match variables to labels, so the prompt for variable foo would be "What is your foo?"
    declare -A vars="($1)"
    for x in ${!vars[@]} ; do
        old_x="${!x}"
        label=${vars[$x]}
        read -p "What is $label? [${!x}] " new_x
        test -n "$new_x" && export $x="$new_x" && echo "$x changed to '${!x}'"
    done
}


function save_config () {
    # parameters are variable tokens:
    #   save_config foo bar
    for x in $* ; do
        # if it's in the config, update it.
        if [ -f $_LCX_config ] && grep -q "^$x=" $_LCX_config ; then
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

    # Manually reset OPTIND - it's a global that only gets reset when the shell restarts.
    OPTIND=1
    local f="($1)" v="($2)"
    local -A flags=$f values=$v
    # declare -p flags values
    shift 2
    local flag_opt="" value_opt="" value_chars="" x
    for x in "${!flags[@]}" ; do flag_opt="$flag_opt$x" ; done
    for x in "${!values[@]}" ; do value_opt="$value_opt$x:" ; value_chars="$value_chars$x" ; done
    # echo "flags=$flag_opt, values=$value_opt"

    while getopts ":$flag_opt$value_opt" option "$@" ; do
        # echo "option=$option, optarg=$OPTARG, optind=$OPTIND"
        case $option in
            \?) echo "Unknown option -$OPTARG" ;;
            [$flag_opt])
                x=${flags[$option]}
                # 'export' makes these visible outside this function; otherwise, use 'local'
                export $x=${!x:-0}
                export $x=$((${!x} + 1)) ;;
            [$value_chars])
                export ${values[$option]}="$OPTARG" ;;
        esac
    done
    return $OPTIND
}

