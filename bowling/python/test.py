import unittest
from Frame import Frame
from Game import Game

class FrameTest(unittest.TestCase):
    def test_frame_init(self):
        frame = Frame()
        self.assertFalse(frame.is_full())

    def test_invalid_pins(self):
        frame = Frame()
        self.assertRaises(Exception, frame.add_ball, 11)

    def test_invalid_pins_neg(self):
        frame = Frame()
        self.assertRaises(Exception, frame.add_ball, -1)

    def test_is_full_False_AfterOneBall(self):
        frame = Frame()
        frame.add_ball(7)
        self.assertFalse(frame.is_full())

    def test_is_strike_True(self):
        frame = Frame()
        frame.add_ball(10)
        self.assertTrue(frame.is_strike())

    def test_is_strike_False(self):
        frame = Frame()
        frame.add_ball(7)
        self.assertFalse(frame.is_strike())
        frame.add_ball(3)
        self.assertFalse(frame.is_strike())

    def test_is_spare(self):
        frame = Frame()
        frame.add_ball(7)
        self.assertFalse(frame.is_spare())
        frame.add_ball(3)
        self.assertTrue(frame.is_spare())

    def test_invalid_frame(self):
        frame = Frame()
        frame.add_ball(3)
        self.assertRaises(Exception, frame.add_ball, 8)

    def test_additionalBallAfterSpare(self):
        frame = self.get_spare_frame()
        frame.add_ball(4)
        self.assertRaises(Exception, frame.add_ball, 8)

    def test_pin_count_ok(self):
        frame = Frame()
        self.assertFalse(frame.is_pin_count_ok(11))
        self.assertTrue(frame.is_pin_count_ok(8))
        frame.add_ball(8)
        self.assertFalse(frame.is_pin_count_ok(3))
        self.assertTrue(frame.is_pin_count_ok(2))
        self.assertTrue(frame.is_pin_count_ok(1))
        frame.add_ball(1)
        self.assertFalse(frame.is_pin_count_ok(2))
        self.assertFalse(frame.is_pin_count_ok(1))
        
    def test_pin_count_strike(self):
        frame = self.get_strike_frame()
        self.assertTrue(frame.is_pin_count_ok(7))
        frame.add_ball(7)
        self.assertFalse(frame.is_pin_count_ok(10))
        self.assertTrue(frame.is_pin_count_ok(3))
        frame.add_ball(3)
        self.assertFalse(frame.is_pin_count_ok(1))
        
    def test_pin_count_spare(self):
        frame = self.get_spare_frame()
        self.assertTrue(frame.is_pin_count_ok(7))
        frame.add_ball(6)
        self.assertFalse(frame.is_pin_count_ok(1))
        
    def test_pin_count_ok_tooMany(self):
        frame = Frame()
        frame.add_ball(8)
        self.assertFalse(frame.is_pin_count_ok(3))
        
    def test_score(self):
        frame = Frame()
        self.assertEqual(0, frame.score())
        frame.add_ball(8)
        self.assertEqual(8, frame.score())
        frame.add_ball(1)
        self.assertEqual(9, frame.score())
        
    def test_score_strike(self):
        frame = self.get_strike_frame()
        self.assertEqual(10, frame.score())
        frame.add_ball(8)
        self.assertEqual(18, frame.score())
        frame.add_ball(1)
        self.assertEqual(19, frame.score())
        
    def test_score_strike(self):
        frame = self.get_spare_frame()
        self.assertEqual(10, frame.score())
        frame.add_ball(8)
        self.assertEqual(18, frame.score())
        
    def test_is_speical(self):
        self.assertFalse(Frame().is_special())
        self.assertTrue(self.get_strike_frame().is_special())
        self.assertTrue(self.get_spare_frame().is_special())
        
    def get_spare_frame(self):
        frame = Frame()
        frame.add_ball(3)
        frame.add_ball(7)
        return frame
    
    def get_strike_frame(self):
        frame = Frame()
        frame.add_ball(10)
        return frame


class GameTest(unittest.TestCase):
    def test_game_init(self):
        game = Game()
        self.assertEquals(game.current_frame(), 1)
        self.assertEquals(game.score(), 0)

    def test_add_ball(self):
        game = Game()
        game.add_ball(1)
        self.assertEquals(game.current_frame(), 1)
        self.assertEquals(game.score(), 1)

    def test_add_second_ball(self):
        game = Game()
        game.add_ball(1)
        game.add_ball(2)
        self.assertEquals(game.score(), 3)
        self.assertEquals(game.current_frame(), 2)

    def test_strike(self):
        game = Game()
        game.add_ball(10)
        self.assertEquals(game.score(), 10)
        self.assertEquals(game.current_frame(), 2)
    
    def check_full_game(self, expectedScore, balls):
        game = Game()
        for ball in balls:
            game.add_ball(ball)
        self.assertEquals(expectedScore, game.score())
    
    def score_full_game(self, balls):
        game = Game()
        for ball in balls:
            game.add_ball(ball)
        return game.score()
    
    def test_game_full(self):
        self.check_full_game(20, [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        
    def test_game_full2(self):
        self.check_full_game(47, [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 10, 10 ,9])
        
    def test_game_full3(self):
        self.check_full_game(300, [10,10,10,10,10,10,10,10,10,10,10,10])
        
    def test_game_full4(self):
        self.check_full_game(173, [7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 10])

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

    def test_game_full_error_1(self):
        self.assertRaises(Exception, self.score_full_game, [1,1, 12,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1])  # invalid roll
    def test_game_full_error_2(self):
        self.assertRaises(Exception, self.score_full_game, [1,1, 6,-1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1])  # invalid roll
    def test_game_full_error_3(self):
        self.assertRaises(Exception, self.score_full_game, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  9,2])  # invalid extras
    def test_game_full_error_4(self):
        self.assertRaises(Exception, self.score_full_game, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  11])  # invalid extras
    def test_game_full_error_5(self):
        self.assertRaises(Exception, self.score_full_game, [10,  10,  10,  10,  10,  10,  10,  10,  10,  9,1, 11])  # invalid extras
    def test_game_full_error_6(self):
        self.assertRaises(Exception, self.score_full_game, [10,  10,  5,6, 10,  10,  10,  10,  10,  10,  1,1])

if __name__ == "__main__":
    unittest.main()
