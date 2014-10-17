require 'test/unit'
require './pascalstriangle.rb'

class TestPascalsTriangle < Test::Unit::TestCase

=begin
  # First row must be [1]
  def test_scenario_1
    # Arrange (a.k.a. define input)
    n = 1

    # Act (a.k.a. apply transformation)
    row = compute_row(n)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      [1],
      row,
      failure_message = 'The computed first row is not [1] as expected'
    )
  end # def test_scenario_1
=end

=begin
  # Second row must be [1,1]
  def test_scenario_2
    # Arrange (a.k.a. define input)
    n = 2

    # Act (a.k.a. apply transformation)
    row = compute_row(n)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      [1,1],
      row,
      failure_message = 'The computed second row is not [1,1] as expected'
    )
  end # def test_scenario_2
=end

=begin
  # Fifth row must be [1,4,6,4,1]
  def test_scenario_5
    # Arrange (a.k.a. define input)
    n = 5

    # Act (a.k.a. apply transformation)
    row = compute_row(n)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      [1,4,6,4,1],
      row,
      failure_message = 'The computed fifth row is not [1,4,6,4,1] as expected'
    )
  end # def test_scenario_5
=end

=begin
  # The third element of row 1000 should be 498501
  def test_scenario_1000
    # Arrange (a.k.a. define input)
    n = 1000

    # Act (a.k.a. apply transformation)
    row = compute_row(n)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      498501,
      row[2],
      failure_message = 'The computed third element of row 1000 is not 498501 as expected'
    )
  end # def test_scenario_1000
=end

=begin
  def test_invalid_input_scenario
    # Arrange (a.k.a. define input)
    n = '1'

    # Act (a.k.a. apply transformation)
    row = compute_row(n)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert(
      row.nil?,
      failure_message = 'The function does not return nil when presented with invalid input'
    )
  end # def test_invalid_input_scenario
=end

end

