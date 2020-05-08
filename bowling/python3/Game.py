from Frame import Frame

class Game(object):
    def __init__(self):
        self.frames = [Frame() for _ in range(10)]
  
    def add_roll(self, pinCount):
        for frame in self.frames:
            if frame.add_roll(pinCount):
                break
 
    def score(self):
        total = 0
        for frame in self.frames:
            total += frame.score()
        return total
