#!/bin/bash

$ echo $PWD
/home/colin/Development/examples/bash
$ echo ${PWD##*/}
bash
$ echo ${PWD%/*}
/home/colin/Development/examples
$


$ for x in 1 2 3 4 ; do touch test$x.txt ; done
$ ls
test1.txt  test2.txt  test3.txt  test4.txt
$ for x in test*.txt ; do mv $x ${x%.*}.html ; done
$ ls
save_config.sh  test1.html  test2.html  test3.html  test4.html
$ for x in test* ; do x=${x#test} ; echo ${x%.*} ; done
1
2
3
4
$
