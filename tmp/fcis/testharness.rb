#!ruby
# encoding: utf-8

#require 'test/unit'
require './topology.rb'

class Topo
  include Topology

  # Board positions
  def self.board_positions_readonly
    @@positions
  end

  # Pacman's next position when moving forward
  def self.pnfp_readonly(current_position_readonly)
    next_position = @@pacman_forward_path[current_position_readonly]
  end

  # Pacman's next position when moving backwards
  def self.pnbp_readonly(current_position_readonly)
    next_position = @@pacman_backward_path[current_position_readonly]
  end

  # Ghost's next position
  def self.gnp_readonly(current_position_readonly)
    next_position = @@ghost_path[current_position_readonly]
  end

  # Neighboring positions
  def self.neighbors_readonly(pos_readonly)
    n = @@neighbors[pos_readonly]
  end
end

class GamePlay
  def self.on_clock_tick(game_state_dcme, clock_ticks_between_ghost_toxicity_readonly)
    # Deep copy
    game_state = { :pacman_is_alive             => game_state_dcme[:pacman_is_alive],
                   :pacman_location             => game_state_dcme[:pacman_location],
                   :ghost_location              => game_state_dcme[:ghost_location],
                   :pacman_direction            => game_state_dcme[:pacman_direction],
                   :ghost_is_toxic              => game_state_dcme[:ghost_is_toxic],
                   :countdown_to_ghost_toxicity => game_state_dcme[:countdown_to_ghost_toxicity],
                   :pellet_locations            => game_state_dcme[:pellet_locations].dup,
                   :score                       => game_state_dcme[:score] }

    # Pacman
    if game_state[:pacman_location] == :a5
      game_state[:pacman_direction] = :forward
    end
    if game_state[:pacman_location] == :i1
      game_state[:pacman_direction] = :backward
    end
    if game_state[:pacman_direction] == :forward
      game_state[:pacman_location] = Topo.pnfp_readonly(game_state[:pacman_location])
    end
    if game_state[:pacman_direction] == :backward
      game_state[:pacman_location] = Topo.pnbp_readonly(game_state[:pacman_location])
    end

    # Ghost
    game_state[:ghost_location] = Topo.gnp_readonly(game_state[:ghost_location])
    game_state[:ghost_is_toxic] = false
    game_state[:countdown_to_ghost_toxicity] -= 1
    if game_state[:countdown_to_ghost_toxicity] == 0
      game_state[:countdown_to_ghost_toxicity] = clock_ticks_between_ghost_toxicity_readonly
      game_state[:ghost_is_toxic] = true
      if !game_state[:pellet_locations].include?(game_state[:ghost_location])
        game_state[:pellet_locations] << game_state[:ghost_location]
      end
    end

    # Interaction between Pacman and Ghost
    if game_state[:ghost_is_toxic]
      if game_state[:pacman_location] == game_state[:ghost_location] or
           Topo.neighbors_readonly(game_state[:ghost_location]).include?(game_state[:pacman_location])
        game_state[:pacman_is_alive] = false
      end
    end

    # Interaction between Pacman and pellet
    if game_state[:pacman_is_alive] and
         game_state[:pellet_locations].include?(game_state[:pacman_location])
      game_state[:pellet_locations] -= [game_state[:pacman_location]]
      game_state[:score] += 1
    end

    game_state
  end

  def self.io_draw_five(board_readonly, first_readonly, second_readonly, third_readonly, fourth_readonly, fifth_readonly)
    output = "     "
    output += board_readonly[first_readonly] + "  "
    output += board_readonly[second_readonly] + "  "
    output += board_readonly[third_readonly] + "  "
    output += board_readonly[fourth_readonly] + "  "
    output += board_readonly[fifth_readonly]
    puts output
  end

  def self.io_draw_two(board_readonly, first_readonly, second_readonly)
    output = "     "
    output += board_readonly[first_readonly] + "           "
    output += board_readonly[second_readonly]
    puts output
  end

  def self.io_draw_board(game_state_readonly)
    board = {}
    Topo.board_positions_readonly.each do |position_readonly|
      board[position_readonly] = "·"
    end
    game_state_readonly[:pellet_locations].each do |pl_readonly|
      board[pl_readonly] = "○"
    end
    board[game_state_readonly[:pacman_location]] = "P"
    board[game_state_readonly[:ghost_location]] = "G"
    if game_state_readonly[:ghost_is_toxic]
      board[game_state_readonly[:ghost_location]] = "☠"
    end

    system("clear")
    puts
    puts
    puts
    io_draw_five(board, :a1, :a2, :a3, :a4, :a5)
    io_draw_two(board, :b1, :b5)
    io_draw_two(board, :c1, :c5)
    io_draw_two(board, :d1, :d5)
    io_draw_five(board, :e1, :e2, :e3, :e4, :e5)
    io_draw_two(board, :f1, :f5)
    io_draw_two(board, :g1, :g5)
    io_draw_two(board, :h1, :h5)
    io_draw_five(board, :i1, :i2, :i3, :i4, :i5)
    puts
    puts "     Score: #{game_state_readonly[:score]}"
    puts
  end

  def self.io_start
    system("clear")
    puts
    print "     Ghost becomes toxic every how many clock ticks: "
    clock_ticks_between_ghost_toxicity = gets.chomp.to_i
    game_state = { :pacman_is_alive => true,
                   :pacman_location => :a5,
                   :ghost_location => :i1,
                   :pacman_direction => :forward,
                   :ghost_is_toxic => false,
                   :countdown_to_ghost_toxicity => clock_ticks_between_ghost_toxicity,
                   :pellet_locations => [],
                   :score => 0 }
    io_draw_board(game_state)
    sleep(1.0)
    until !game_state[:pacman_is_alive]
      sleep(0.4)
      game_state = GamePlay.on_clock_tick(game_state, clock_ticks_between_ghost_toxicity)
      io_draw_board(game_state)
    end
  end
end

#class TestPacman < Test::Unit::TestCase
#  def setup
#  end
#
#  def on_clock_tick(initial_game_state)
#  end
#
#  def test_that_after_one_clock_tick_pacman_is_in_board_position_a4
#    game_state = { :pacman_location => :a5,
#                   :ghost_location => :i1,
#                   :pacman_direction => :forward,
#                   :ghost_is_toxic => false,
#                   :countdown_to_ghost_toxicity => 7,
#                   :pellet_locations => [],
#                   :score => 0 }
#
#    game_state = GamePlay.on_clock_tick(game_state)
#
#    assert_equal( :a4, game_state[:pacman_location],
#                  failure_message = "Pacman is not in board position a4 after one clock tick" )
#  end
#
#  def test_that_after_two_clock_ticks_pacman_is_in_board_position_a3
#    game_state = { :pacman_location => :a5,
#                   :ghost_location => :i1,
#                   :pacman_direction => :forward,
#                   :ghost_is_toxic => false,
#                   :countdown_to_ghost_toxicity => 7,
#                   :pellet_locations => [],
#                   :score => 0 }
#
#    game_state = GamePlay.on_clock_tick(game_state)
#    game_state = GamePlay.on_clock_tick(game_state)
#
#    assert_equal( :a3, game_state[:pacman_location],
#                  failure_message = "Pacman is not in board position a3 after two clock ticks" )
#  end
#
#  def test_that_after_being_at_i1_pacman_goes_to_i2
#    game_state = { :pacman_location => :i1,
#                   :ghost_location => :e1,
#                   :pacman_direction => :forward,
#                   :ghost_is_toxic => false,
#                   :countdown_to_ghost_toxicity => 7,
#                   :pellet_locations => [],
#                   :score => 0 }
#
#    game_state = GamePlay.on_clock_tick(game_state)
#
#    assert_equal( :i2, game_state[:pacman_location],
#                  failure_message = "Pacman does not go to i2 after being at i1" )
#    assert_equal( :backward, game_state[:pacman_direction],
#                  failure_message = "Pacman does not reverse direction after reaching i1" )
#  end
#
#  def test_that_after_one_clock_tick_ghost_is_in_board_position_i2
#    game_state = { :pacman_location => :a5,
#                   :ghost_location => :i1,
#                   :pacman_direction => :forward,
#                   :ghost_is_toxic => false,
#                   :countdown_to_ghost_toxicity => 7,
#                   :pellet_locations => [],
#                   :score => 0 }
#
#    game_state = GamePlay.on_clock_tick(game_state)
#
#    assert_equal( :i2, game_state[:ghost_location],
#                  failure_message = "Ghost is not in board position i2 after one clock tick" )
#  end
#
#  def test_that_after_a_given_number_of_clock_ticks_ghost_is_toxic
#    game_state = { :pacman_location => :a5,
#                   :ghost_location => :i1,
#                   :pacman_direction => :forward,
#                   :ghost_is_toxic => false,
#                   :countdown_to_ghost_toxicity => 7,
#                   :pellet_locations => [],
#                   :score => 0 }
#
#    6.times do
#      game_state = GamePlay.on_clock_tick(game_state)
#    end
#
#    assert( !game_state[:ghost_is_toxic],
#            failure_message = "Ghost is toxic when he should not be" )
#
#    game_state = GamePlay.on_clock_tick(game_state)
#
#    assert( game_state[:ghost_is_toxic],
#            failure_message = "Ghost is not toxic when he should be" )
#
#    game_state = GamePlay.on_clock_tick(game_state)
#
#    assert( !game_state[:ghost_is_toxic],
#            failure_message = "Ghost is toxic when he should not be" )
#
#  end
#
#  def test_the_combination_of_proximity_and_toxicity
#    game_state = { :pacman_is_alive => true,
#                   :pacman_location => :h5,
#                   :ghost_location => :i4,
#                   :pacman_direction => :forward,
#                   :ghost_is_toxic => false,
#                   :countdown_to_ghost_toxicity => 1,
#                   :pellet_locations => [],
#                   :score => 0 }
#
#    game_state = GamePlay.on_clock_tick(game_state)
#
#    assert( !game_state[:pacman_is_alive],
#            failure_message = "Fail 1: Pacman is alive when he should not be" )
#
#    game_state = { :pacman_is_alive => true,
#                   :pacman_location => :i5,
#                   :ghost_location => :i4,
#                   :pacman_direction => :forward,
#                   :ghost_is_toxic => false,
#                   :countdown_to_ghost_toxicity => 1,
#                   :pellet_locations => [],
#                   :score => 0 }
#
#    game_state = GamePlay.on_clock_tick(game_state)
#
#    assert( !game_state[:pacman_is_alive],
#            failure_message = "Fail 2: Pacman is alive when he should not be" )
#
#    game_state = { :pacman_is_alive => true,
#                   :pacman_location => :g5,
#                   :ghost_location => :i4,
#                   :pacman_direction => :forward,
#                   :ghost_is_toxic => false,
#                   :countdown_to_ghost_toxicity => 1,
#                   :pellet_locations => [],
#                   :score => 0 }
#
#    game_state = GamePlay.on_clock_tick(game_state)
#
#    assert( !game_state[:pacman_is_alive],
#            failure_message = "Fail 3: Pacman is alive when he should not be" )
#
#    game_state = { :pacman_is_alive => true,
#                   :pacman_location => :i4,
#                   :ghost_location => :i4,
#                   :pacman_direction => :forward,
#                   :ghost_is_toxic => false,
#                   :countdown_to_ghost_toxicity => 1,
#                   :pellet_locations => [],
#                   :score => 0 }
#
#    game_state = GamePlay.on_clock_tick(game_state)
#
#    assert( game_state[:pacman_is_alive],
#            failure_message = "Fail 4: Pacman is dead when he should be alive" )
#
#    game_state = { :pacman_is_alive => true,
#                   :pacman_location => :f5,
#                   :ghost_location => :i4,
#                   :pacman_direction => :forward,
#                   :ghost_is_toxic => false,
#                   :countdown_to_ghost_toxicity => 1,
#                   :pellet_locations => [],
#                   :score => 0 }
#
#    game_state = GamePlay.on_clock_tick(game_state)
#
#    assert( game_state[:pacman_is_alive],
#            failure_message = "Fail 5: Pacman is dead when he should be alive" )
#  end
#
#  def test_that_ghost_leaves_a_pellet_behind_when_toxic
#    game_state = { :pacman_is_alive => true,
#                   :pacman_location => :i2,
#                   :ghost_location => :i4,
#                   :pacman_direction => :forward,
#                   :ghost_is_toxic => false,
#                   :countdown_to_ghost_toxicity => 1,
#                   :pellet_locations => [],
#                   :score => 0 }
#
#    game_state = GamePlay.on_clock_tick(game_state)
#
#    pl = game_state[:pellet_locations];
#    assert( (pl - [:i5]).empty? && ([:i5] - pl).empty?,
#            failure_message = "Ghost does not leave a pellet behind when toxic" )
#  end
#
#  def test_that_pacman_scores_a_point_after_eating_a_pellet
#    game_state = { :pacman_is_alive => true,
#                   :pacman_location => :h5,
#                   :ghost_location => :i2,
#                   :pacman_direction => :forward,
#                   :ghost_is_toxic => false,
#                   :countdown_to_ghost_toxicity => 3,
#                   :pellet_locations => [:i5],
#                   :score => 0 }
#
#    game_state = GamePlay.on_clock_tick(game_state)
#
#    assert_equal( 1, game_state[:score],
#                  failure_message = "The score has not gone up by 1 as expected" )
#
#    assert( game_state[:pellet_locations].empty?,
#            failure_message = "The list of pellets is not empty as expected" )
#  end
#end

GamePlay.io_start

