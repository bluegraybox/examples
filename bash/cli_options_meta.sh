#!/bin/bash

function get_my_opts () {
    # $1 and $2 are strings that evaluate to associative arrays.
    # For example, "([f]=foo [b]=bar)"
    # They match options to variables, so the option "-f baz" causes the variable foo to be set to "baz".
    # Flag options don't take a parameter; we just count how often they appear.
    # So if the above example were the flags option, "-f -f -f" would cause the variable foo to be set to 3.
    local -A _flags=$1 _values=$2
    shift 2
    local flag_opt="" value_opt="" x
    for x in "${!_flags[@]}" ; do flag_opt="$flag_opt$x" ; done
    for x in "${!_values[@]}" ; do value_opt="$value_opt$x:" ; done

    while getopts ":$flag_opt$value_opt" option "$@" ; do
        case $option in
            \?) echo "Unknown option -$OPTARG" ;;
            [$flag_opt])
                x=${_flags[$option]}
                # 'export' makes these visible outside this function; otherwise, use 'local'
                export $x=${!x:-0}
                export $x=$((${!x} + 1)) ;;
            [$value_opt]) export ${_values[$option]}="$OPTARG" ;;
        esac
    done
    return $OPTIND
}

function main () {
    # Strings that evaluate to associative arrays
    local flags="([d]=debug [h]=show_help)" values="([f]=foo [b]=bar)"
    get_my_opts "$flags" "$values" "$@"
    local used=$?  # grab the return value
    shift $((used - 1))
    for x in debug show_help foo bar ; do echo "$x=${!x}" ; done
    echo "args=$@"
}

main "$@"

