#!/usr/bin/ruby

require File.join(File.dirname(__FILE__), 'indent_parser')

# filename = ARGV.shift()
parser = TextParser.new
# parser.process_file filename
parser.process_file

