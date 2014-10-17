require 'test/unit'
require './abstractpoker.rb'

class TestAbstractPoker < Test::Unit::TestCase

=begin
  # High Card means either three or four consecutive numbers
  # Testing three consecutive numbers now
  def test_high_card_scenario_3
    # Arrange (a.k.a. define input)
    poker_hand = [7,2,6,9,5]

    # Act (a.k.a. apply transformation)
    classified_as = classify_poker_hand(poker_hand)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      :high_card,
      classified_as,
      failure_message = 'A poker hand of [7,2,6,9,5] is not classified as high card'
    )
  end # def test_high_card_scenario_3
=end

=begin
  # High Card means either three or four consecutive numbers
  # Testing four consecutive numbers now
  def test_high_card_scenario_4
    # Arrange (a.k.a. define input)
    poker_hand = [7,4,6,9,5]

    # Act (a.k.a. apply transformation)
    classified_as = classify_poker_hand(poker_hand)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      :high_card,
      classified_as,
      failure_message = 'A poker hand of [7,4,6,9,5] is not classified as high card'
    )
  end # def test_high_card_scenario_4
=end

=begin
  def test_one_pair_scenario
    # Arrange (a.k.a. define input)
    poker_hand = [4,3,5,8,3]

    # Act (a.k.a. apply transformation)
    classified_as = classify_poker_hand(poker_hand)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      :one_pair,
      classified_as,
      failure_message = 'A poker hand of [4,3,5,8,3] is not classified as one pair'
    )
  end # def test_one_pair_scenario
=end

=begin
  def test_two_pairs_scenario
    # Arrange (a.k.a. define input)
    poker_hand = [8,3,5,8,3]

    # Act (a.k.a. apply transformation)
    classified_as = classify_poker_hand(poker_hand)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      :two_pairs,
      classified_as,
      failure_message = 'A poker hand of [8,3,5,8,3] is not classified as two pairs'
    )
  end # def test_two_pairs_scenario
=end

=begin
  def test_three_of_a_kind_scenario
    # Arrange (a.k.a. define input)
    poker_hand = [5,9,5,7,5]

    # Act (a.k.a. apply transformation)
    classified_as = classify_poker_hand(poker_hand)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      :three_of_a_kind,
      classified_as,
      failure_message = 'A poker hand of [5,9,5,7,5] is not classified as three of a kind'
    )
  end # def test_three_of_a_kind_scenario
=end

=begin
  # Straight means five consecutive numbers
  def test_straight_scenario
    # Arrange (a.k.a. define input)
    poker_hand = [7,5,6,8,9]

    # Act (a.k.a. apply transformation)
    classified_as = classify_poker_hand(poker_hand)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      :straight,
      classified_as,
      failure_message = 'A poker hand of [7,5,6,8,9] is not classified as straight'
    )
  end # def test_straight_scenario
=end

=begin
  def test_full_house_scenario
    # Arrange (a.k.a. define input)
    poker_hand = [8,3,3,8,3]

    # Act (a.k.a. apply transformation)
    classified_as = classify_poker_hand(poker_hand)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      :full_house,
      classified_as,
      failure_message = 'A poker hand of [8,3,3,8,3] is not classified as full house'
    )
  end # def test_full_house_scenario
=end

=begin
  def test_four_of_a_kind_scenario
    # Arrange (a.k.a. define input)
    poker_hand = [5,5,5,2,5]

    # Act (a.k.a. apply transformation)
    classified_as = classify_poker_hand(poker_hand)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      :four_of_a_kind,
      classified_as,
      failure_message = 'A poker hand of [5,5,5,2,5] is not classified as four of a kind'
    )
  end # def test_four_of_a_kind_scenario
=end

=begin
  # Just a regular/ordinary poker hand
  def test_valid_but_nothing_special_scenario
    # Arrange (a.k.a. define input)
    poker_hand = [2,9,5,7,3]

    # Act (a.k.a. apply transformation)
    classified_as = classify_poker_hand(poker_hand)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      :valid_but_nothing_special,
      classified_as,
      failure_message = 'A poker hand of [2,9,5,7,3] is not classified as valid but nothing special'
    )
  end # def test_valid_but_nothing_special_scenario
=end

=begin
  # It is not possible to have a poker hand with 5 identical cards
  def test_impossible_hand_scenario
    # Arrange (a.k.a. define input)
    poker_hand = [7,7,7,7,7]

    # Act (a.k.a. apply transformation)
    classified_as = classify_poker_hand(poker_hand)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      :impossible_hand,
      classified_as,
      failure_message = 'A poker hand of [7,7,7,7,7] is not classified as impossible hand'
    )
  end # def test_impossible_hand_scenario
=end

=begin
  # At least one card is out of bounds
  def test_at_least_one_card_is_out_of_bounds_scenario
    # Arrange (a.k.a. define input)
    poker_hand = [5,1500,5,7,5]

    # Act (a.k.a. apply transformation)
    classified_as = classify_poker_hand(poker_hand)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      :at_least_one_card_is_out_of_bounds,
      classified_as,
      failure_message = 'A poker hand of [5,1500,5,7,5] is not classified as at least one card is out of bounds'
    )
  end # def test_at_least_one_card_is_out_of_bounds_scenario
=end

=begin
  # At least one card is not an integer
  def test_at_least_one_card_is_not_an_integer_scenario
    # Arrange (a.k.a. define input)
    poker_hand = [5,'9',5,7,5]

    # Act (a.k.a. apply transformation)
    classified_as = classify_poker_hand(poker_hand)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      :at_least_one_card_is_not_an_integer,
      classified_as,
      failure_message = 'A poker hand with a string in it is not classified as at least one card is not an integer'
    )
  end # def test_at_least_one_card_is_not_an_integer_scenario
=end

=begin
  # A poker hand should have exactly 5 cards
  def test_too_many_or_too_few_cards_scenario
    # Arrange (a.k.a. define input)
    poker_hand = [5,9,5,7,5,1,1,1]

    # Act (a.k.a. apply transformation)
    classified_as = classify_poker_hand(poker_hand)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      :too_many_or_too_few_cards,
      classified_as,
      failure_message = 'A poker hand of [5,9,5,7,5,1,1,1] is not classified as too many or too few cards'
    )
  end # def test_too_many_or_too_few_cards_scenario
=end

=begin
  # A poker hand needs to be a Ruby array
  def test_not_a_ruby_array_scenario
    # Arrange (a.k.a. define input)
    poker_hand = {}

    # Act (a.k.a. apply transformation)
    classified_as = classify_poker_hand(poker_hand)

    # Assert
    # (a.k.a. state our expectation about the result of the transformation)
    assert_equal(
      :not_a_ruby_array,
      classified_as,
      failure_message = 'A poker hand of {} is not classified as not a Ruby array'
    )
  end # def test_not_a_ruby_array_scenario
=end

end # class TestAbstractPoker

