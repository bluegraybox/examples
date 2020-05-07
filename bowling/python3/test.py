import unittest
from Frame import Frame
from Game import Game

class FrameTest(unittest.TestCase):
    def check_full_game(self, expectedScore, rolls):
        game = Game()
        for pinCount in rolls:
            game.add_roll(pinCount)
        self.assertEqual(expectedScore, game.score())

    def test_game_full_suite(self):
        self.check_full_game(0,   [0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0])
        self.check_full_game(20,  [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1])
        self.check_full_game(6,   [1,1, 1,1, 1,1])  # incomplete
        self.check_full_game(18,  [1,1, 6,4, 3])  # incomplete w/ spare
        self.check_full_game(150, [5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5])
        self.check_full_game(47,  [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 10, 10 ,9])
        self.check_full_game(173, [7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 10])
        self.check_full_game(300, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10])
        self.check_full_game(280, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  5])  # incomplete
        self.check_full_game(300, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10, 10, 10, 10])  # extras
        self.check_full_game(240, [10,  10,  10,  0,0, 10,  10,  10,  10,  10,  10,  10,  10])
        self.check_full_game(245, [10,  10,  10,  10,  10,  10,  10,  10,  10,  1,1])

if __name__ == "__main__":
    unittest.main()
