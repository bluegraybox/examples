class Frame(object):
    def __init__(self):
        self.rolls = []
        self.bonusRolls = []

    # Return false if this frame is already full.
    def add_roll(self, pinCount):
        if self.__is_strike():
            if len(self.bonusRolls) < 2:
                self.bonusRolls.append(pinCount)
            return False
        elif self.__is_spare():
            if not self.bonusRolls:
                self.bonusRolls.append(pinCount)
            return False
        elif len(self.rolls) < 2:
            self.rolls.append(pinCount)
            return True
        else:
            return False

    def __is_strike(self):
        return len(self.rolls) == 1 and sum(self.rolls) == 10

    def __is_spare(self):
        return len(self.rolls) == 2 and sum(self.rolls) == 10

    def score(self):
        return sum(self.rolls) + sum(self.bonusRolls)
