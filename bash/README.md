# Bash Scripts

These are various snippets of bash code. Some are complete scripts, some aren't - they just demonstrate techniques that I use with some frequency. They're tiny bits of code, but they often have some bit of obscure syntax or fiddly edge cases that are easy to forget.

### lib\_config.sh

This is a utility for managing configuration settings for scripts. It has functions for parsing command-line options, saving variables to a config file and reloading them, and prompting the user to change them. See **template.sh** for how to use it.

Unit tests for it are in **test/test\_lib\_config.sh**.

Consider it alpha quality. It works fine, but it's very fussy about how things are quoted. That's just bash.

### Others
**trap\_demo.sh**
Just shows how to trap a ctrl-c so your script can perform cleanup before exiting.

