# Fizzbuzz

Fizzbuzz is about the simplest programming challenge imaginable, but [it's been observed](http://imranontech.com/2007/01/24/using-fizzbuzz-to-find-developers-who-grok-coding/) that a surprising number of seemingly experienced programmers choke when told to actually sit down and code it. Of course, telling any programmer this [pretty much compels us](http://www.codinghorror.com/blog/2007/02/fizzbuzz-the-programmers-stairway-to-heaven.html) to sit down and code it just to prove (to ourselves at least) that we can. Since I've been tinkering in Erlang, I figured I'd try that. The interesting thing is that even for something this simple, you have to do it differently in Erlang. In fact, it's instructive to see _how_ differently you can do it in Erlang.

So for reference, here's the Python version:

```python
#!/usr/bin/python

max = 100
for x in range(1,max+1):
    if ((x % 5) == 0) and ((x % 3) == 0):
        print "fizzbuzz"
    elif (x % 3) == 0:
        print "fizz"
    elif ((x % 5) == 0):
        print "buzz"
    else:
        print x
```

Here's my first draft of the Erlang version

```erlang
#!/usr/local/bin/escript
-mode(compile).

main([]) -> fizzbuzz(1).

fizzbuzz(X) when X > 100 ->
    ok;
fizzbuzz(X) when ((X rem 5) == 0) and ((X rem 3) == 0) ->
    io:format("fizzbuzz~n"),
    fizzbuzz(X + 1);
fizzbuzz(X) when ((X rem 3) == 0) ->
    io:format("fizz~n"),
    fizzbuzz(X + 1);
fizzbuzz(X) when ((X rem 5) == 0) ->
    io:format("buzz~n"),
    fizzbuzz(X + 1);
fizzbuzz(X) ->
    io:format("~p~n", [X]),
    fizzbuzz(X + 1).
```

But really, that's not very Erlang-y, is it? That version is all about the side effects. Let's split out the printing, and see what we get.

```erlang
#!/usr/local/bin/escript
-mode(compile).

main([]) ->
    Out = fizzbuzz(1, []),
    print(Out).

fizzbuzz(X, Out) when X > 100 ->
    lists:reverse(Out);
fizzbuzz(X, Out) when ((X rem 3) == 0) and ((X rem 5) == 0) ->
    fizzbuzz(X + 1, [fizzbuzz|Out]);
fizzbuzz(X, Out) when (X rem 3) == 0 ->
    fizzbuzz(X + 1, [fizz|Out]);
fizzbuzz(X, Out) when (X rem 5) == 0 ->
    fizzbuzz(X + 1, [buzz|Out]);
fizzbuzz(X, Out) ->
    fizzbuzz(X + 1, [X|Out]).

print([]) -> ok;
print([First|Rest]) ->
    io:format("~p~n", [First]),
    print(Rest).
```

Ok, that's a bit better, but now that I think about it, the fizzbuzz function itself isn't recursive. Each value can be calculated independently. fizzbuzz(6) is fizz, whether it appears in a sequence or not. We have the fizzbuzz calculation mashed together with iterating over a range. Let's refactor that so that the list printing function handles the interation instead.

```erlang
#!/usr/local/bin/escript
-mode(compile).

main([]) ->
    print_loop(1).

print_loop(I) when I > 100 ->
    ok;
print_loop(I) ->
    io:format("~p~n", [fizzbuzz(I)]),
    print_loop(I + 1).

fizzbuzz(X) when ((X rem 5) == 0) and ((X rem 3) == 0) -> fizzbuzz;
fizzbuzz(X) when ((X rem 3) == 0) -> fizz;
fizzbuzz(X) when ((X rem 5) == 0) -> buzz;
fizzbuzz(X) -> X.

```

Now the fizzbuzz function itself is very declarative, and the looping is cleaner and simpler. This separation lets us parameterize the print_loop without modifying the fizzbuzz rules. That's kinda nice.

```erlang
#!/usr/local/bin/escript
-mode(compile).


main([]) ->
    print_loop(fun fizzbuzz/1, 100, 1).

print_loop(_Func, Max, I) when I > Max ->
    ok;
print_loop(Func, Max, I) ->
    io:format("~p~n", [Func(I)]),
    print_loop(Func, Max, I + 1).

fizzbuzz(X) when ((X rem 5) == 0) and ((X rem 3) == 0) -> fizzbuzz;
fizzbuzz(X) when ((X rem 3) == 0) -> fizz;
fizzbuzz(X) when ((X rem 5) == 0) -> buzz;
fizzbuzz(X) -> X.
```

The end result is about the same number of lines of code as the Python version, so I can't say that it's more efficient. But it's surprisingly _different_. You can go back and re-write the Python version to have that functional separation, but it doesn't feel the same.

```python
#!/usr/bin/python

def fizzbuzz(x):
    if ((x % 5) == 0) and ((x % 3) == 0):
        return "fizzbuzz"
    elif (x % 3) == 0:
        return "fizz"
    elif ((x % 5) == 0):
        return "buzz"
    else:
        return x

max = 100
for x in range(1,max+1):
        print fizzbuzz(x)
```

And would you really? The original Python version looks fine; the second one actually looks weird, with its multiple returns. It's not just a matter of what's possible in a language; it's what's easy, and what's common in its community.

