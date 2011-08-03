from Frame import Frame

class Game(object):
    def __init__(self):
        self.frames = [Frame()]
  
    def add_ball(self, pins):
        for frame in self.frames:
            if frame.is_pin_count_ok(pins):
                frame.add_ball(pins)
 
        current_frame = self.frames[-1]
        if current_frame.is_full() and len(self.frames) < 10:
            self.frames.append(Frame())
 
    def current_frame(self):
        return len(self.frames)

    def score(self):
        total = 0
        for frameToScore in self.frames:
            total += frameToScore.score()
        return total

