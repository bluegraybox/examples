#!/usr/bin/ruby


class Line
    attr_reader :content, :indent

    def initialize(text)
        text.rstrip!
        # this will get screwed up by a mix of tabs and spaces, but is ok with either.
        l = text.length
        @content = text.lstrip
        @indent = l - @content.length
    end
end

class Node
    def initialize(parent=nil)
        @parent = parent
        @children = []
    end

    def add(child)
        @children.push child
    end

    def parents
        @parent ?  [self] + @parent.parents : [self]
    end

    def leaf?
        @children.length <= 0
    end

    def walk(action)
        if action.respond_to? :child
            sub_act = action.child
        else
            sub_act = action
        end
        @children.each do |c|
            action.call c
            # depth first
            c.walk sub_act
        end
    end
end

class TextNode < Node
    attr_reader :content

    def initialize(text, parent=nil)
        super(parent)
        @content = text.strip
    end
end

class Tree
    def initialize(node_class)
        @node_class = node_class
        @root = Node.new
        @branch = []
    end

    def add(line)
        # @indent = line.indent unless @indent
        while @branch.length > 0 and line.indent <= @branch[-1][:indent] do
            @branch.pop
        end
        if @branch[-1]
            node = @node_class.new line.content, @branch[-1][:node]
            @branch[-1][:node].add node
        else
            node = @node_class.new line.content
            @root.add node
        end
        @branch.push( {:indent => line.indent, :node => node} )
    end

    def walk(action)
        @root.walk action
    end
end

class RecursiveAction
    attr_accessor :level

    def initialize(action)
        @level = 0
        @action = action
    end

    def child
        c = RecursiveAction.new @action
        c.level = @level + 1
        c
    end

    def call(*args)
        @action.call @level, *args
    end
end

class TextParser
    # def process_file(filename)
    def process_file()
        tree = Tree.new TextNode
        ARGF.each() { |line| tree.add Line.new(line) }
        # act = RecursiveAction.new proc { |indent,node| puts "    " * indent + node.content }
        out = Proc.new do |indent, node|
            c = node.parents().reverse()[0..-2].map { |n| n.content }
            c.push node.content
            # puts "    " * indent + c.join("::")
            if node.leaf?
                puts c.join("::")
            end
        end
        act = RecursiveAction.new out
        tree.walk act
    end
end

