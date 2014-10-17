require 'test/unit'
require './cgol.rb'

class TestConwaysGameOfLife < Test::Unit::TestCase

=begin
  def test_with_an_empty_seed
    # Arrange (a.k.a. define input)
    seed = [].to_set

    # Act (a.k.a. apply transformation)
    alive_cells_in_new_generation = evolve_universe(seed)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      [].to_set,
      alive_cells_in_new_generation,
      failure_message = "An empty seed does not evolve into an empty seed as expected"
    )
  end # def test_with_an_empty_seed
=end

=begin
  def test_with_a_seed_of_00
    # Arrange (a.k.a. define input)
    seed = [Cell.new(0,0)].to_set

    # Act (a.k.a. apply transformation)
    alive_cells_in_new_generation = evolve_universe(seed)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      [].to_set,
      alive_cells_in_new_generation,
      failure_message = "A seed of [(0,0)] does not evolve into an empty seed as expected"
    )
  end # def test_with_a_seed_of_00
=end

=begin
  def test_with_a_seed_of_00_01
    # Arrange (a.k.a. define input)
    seed =
      [ Cell.new(0,0),
        Cell.new(0,1)
      ].to_set

    # Act (a.k.a. apply transformation)
    alive_cells_in_new_generation = evolve_universe(seed)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      [].to_set,
      alive_cells_in_new_generation,
      failure_message = "A seed of [(0,0),(0,1)] does not evolve into an empty seed as expected"
    )
  end # def test_with_a_seed_of_00_01
=end

=begin
  def test_with_a_seed_of_00_01_10
    # Arrange (a.k.a. define input)
    seed =
      [ Cell.new(0,0),
        Cell.new(0,1),
        Cell.new(1,0)
      ].to_set

    # Act (a.k.a. apply transformation)
    alive_cells_in_new_generation = evolve_universe(seed)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    expected_alive_cells_in_new_generation =
      [ Cell.new(0,0),
        Cell.new(0,1),
        Cell.new(1,0),
        Cell.new(1,1)
      ].to_set

    s = '[(0,0),(0,1),(1,0)]'
    e = '[(0,0),(0,1),(1,0),(1,1)]'
    assert_equal(
      expected_alive_cells_in_new_generation,
      alive_cells_in_new_generation,
      failure_message = "A seed of #{s} does not evolve into #{e} as expected"
    )
  end # def test_with_a_seed_of_00_01_10
=end

=begin
  def test_with_a_seed_of_10_21_02_12_22
    # Arrange (a.k.a. define input)
    seed =
      [ Cell.new(1,0),
        Cell.new(2,1),
        Cell.new(0,2),
        Cell.new(1,2),
        Cell.new(2,2)
      ].to_set

    # Act (a.k.a. apply transformation)
    alive_cells_in_new_generation = evolve_universe(seed)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    expected_alive_cells_in_new_generation =
      [ Cell.new(0,1),
        Cell.new(1,2),
        Cell.new(1,3),
        Cell.new(2,1),
        Cell.new(2,2)
      ].to_set

    s = '[(1,0),(2,1),(0,2),(1,2),(2,2)]'
    e = '[(0,1),(1,2),(1,3),(2,1),(2,2)]'
    assert_equal(
      expected_alive_cells_in_new_generation,
      alive_cells_in_new_generation,
      failure_message = "A seed of #{s} does not evolve into #{e} as expected"
    )
  end # def test_with_a_seed_of_10_21_02_12_22
=end

=begin
  def test_with_a_seed_of_01_12_13_21_22
    # Arrange (a.k.a. define input)
    seed =
      [ Cell.new(0,1),
        Cell.new(1,2),
        Cell.new(1,3),
        Cell.new(2,1),
        Cell.new(2,2)
      ].to_set

    # Act (a.k.a. apply transformation)
    alive_cells_in_new_generation = evolve_universe(seed)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    expected_alive_cells_in_new_generation =
      [ Cell.new(0,2),
        Cell.new(1,3),
        Cell.new(2,1),
        Cell.new(2,2),
        Cell.new(2,3)
      ].to_set

    s = '[(0,1),(1,2),(1,3),(2,1),(2,2)]'
    e = '[(0,2),(1,3),(2,1),(2,2),(2,3)]'
    assert_equal(
      expected_alive_cells_in_new_generation,
      alive_cells_in_new_generation,
      failure_message = "A seed of #{s} does not evolve into #{e} as expected"
    )
  end # def test_with_a_seed_of_01_12_13_21_22
=end

end

