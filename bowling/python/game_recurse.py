#!/usr/bin/python

# This is the Erlang solution, translated into Python.

def score(rolls, frame=1, total=0):
    if frame > 10 or not rolls:
        return total
    elif rolls[0] == 10:
        rolls = rolls[1:]
        bonus = sum(rolls[0:2]) if rolls else 0
        return score(rolls, frame + 1, total + 10 + bonus)
    elif len(rolls) == 1:
        return total + rolls[0]
    elif sum(rolls[0:2]) == 10:
        rolls = rolls[2:]
        bonus = rolls[0] if rolls else 0
        return score(rolls, frame + 1, total + 10 + bonus)
    else:
        total += sum(rolls[0:2])
        return score(rolls[2:], frame + 1, total)


def test():
    tests = [
        [0,   [0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0]],
        [20,  [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1]],
        [6,   [1,1, 1,1, 1,1]], # incomplete
        [18,  [1,1, 6,4, 3]], # incomplete w/ spare
        [150, [5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5]],
        [47,  [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 10, 10 ,9]],
        [173, [7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 10]],
        [300, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10]],
        [280, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  5]],  # incomplete
        [300, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10, 10, 10, 10]], # extras
        [240, [10,  10,  10,  0,0, 10,  10,  10,  10,  10,  10,  10,  10]],
        [245, [10,  10,  10,  10,  10,  10,  10,  10,  10,  1,1]]]
    run_tests(tests)

def run_tests(tests, passes=0, fails=0):
    if not tests:
        if fails:
            print("Failed! %d fail, %d pass" % (fails, passes))
        else:
            print("Passed! %d tests" % (passes))
    else:
        test = tests[0]
        expected, rolls = test
        total = score(rolls)
        if total == expected:
            print(".")
            run_tests(tests[1:], passes + 1, fails)
        else:
            print("Fail: expected=%d, scored=%d" % (expected, total))
            run_tests(tests[1:], passes, fails + 1)

if __name__ == "__main__":
    test()

