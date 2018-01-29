# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

# == Enhancements ==
#
# 1. In your game, the player can press the ’u’ key to make the piece that is
# falling rotate 180 degrees. (Note it is normal for this to make some pieces
# appear to move slightly.)
#
# 2. In your game, instead of the pieces being randomly (and uniformly) chosen
# from the 7 classic pieces, the pieces are randomly (and uniformly) chosen
# from 10 pieces. They are the classic 7 and these 3:
#
#       ++            +
#       +++   +++++   ++
#
# The initial rotation for each piece is also chosen randomly.
#
# 3. In your game, the player can press the ’c’ key to cheat: If the score is
# less than 100, nothing happens. Else the player loses 100 points (cheating
# costs you) and the next piece that appears will be:
#
#       +
#
# The piece after is again chosen randomly from the 10 above (unless, of
# course, the player hits ’c’ while the “cheat piece” is falling and still has
# a large enough score). Hitting ’c’ multiple times while a single piece is
# falling should behave no differently than hitting it once.

class MyPiece < Piece
  # All_My_Pieces = All_Pieces + [
  All_My_Pieces = [
    # rotations([[0, 0], [], [], [], []]),
    # [[[0, 0], [0, 1], [1, 1]]],
    # rotations([[0, 0], [1, 0], [0, 1], [1, 1]]),

    rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 0]]),
    rotations([[0, 0], [1, 0], [2, 0], [3, 0], [4, 0]]),
    rotations([[0, 0], [0, -1], [1, -1]]),

    # rotations([[0, 0], [0, 1], [1, 1]])
    # [[[0, 0], [0, 1], [1, 1]]]
    # rotations([[0, 0], [1, 0], [0, 1], [1, 1]]),
  ]

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

class MyBoard < Board
  def initialize (game)
    super
    next_piece
  end

  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end

  def rotate_180_degrees
    rotate_counter_clockwise
    rotate_counter_clockwise
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position

    (0..3).each{|index|
      unless @current_pos.nil? || @current_pos[index].nil?
        current = locations[index]
        @grid[current[1]+displacement[1]][current[0]+displacement[0]] = @current_pos[index]
      end
    }

    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    @root.bind('u', proc {@board.rotate_180_degrees})
    super
  end
end
