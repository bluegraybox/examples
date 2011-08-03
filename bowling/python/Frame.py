class Frame(object):
	def __init__(self):
		self.balls = []
		self.extraBalls = []

	def is_strike(self):
		return len(self.balls) == 1 and sum(self.balls) == 10

	def is_spare(self):
		return len(self.balls) == 2 and sum(self.balls) == 10

	def add_ball(self, pins):
		if self.is_pin_count_ok(pins):
			if self.is_spare() or self.is_strike():
				self.extraBalls.append(pins)
			else:
				self.balls.append(pins)
			return
		else:
			raise Exception("Unable to add ball")

	def is_special(self):
		return self.is_spare() or self.is_strike()
	
	def is_pin_count_ok(self, pins):
		if pins > 10:
			return False
		elif self.is_strike() and len(self.extraBalls) < 2:
			return True
		elif self.is_spare() and len(self.extraBalls) < 1:
			return True
		elif len(self.balls) == 2:
			return False
		elif (sum(self.balls) + pins > 10):
			return False
		return True

	def is_full(self):
		return len(self.balls) == 2 or sum(self.balls) == 10

	def score(self):
		return sum(self.balls) + sum(self.extraBalls)