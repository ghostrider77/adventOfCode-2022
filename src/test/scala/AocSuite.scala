import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AocSuite extends AnyFreeSpec, Matchers:
  "Day 01" - {
    val calories: List[List[Int]] =
      List(List(1000, 2000, 3000), List(4000), List(5000, 6000), List(7000, 8000, 9000), List(10000))

    "Part 1" - {
      import Day01a.getMaximalCalories

      "should find the total calories that the Elf with the largest number of calories carries" in {
        getMaximalCalories(calories) shouldEqual 24000
      }
    }

    "Part 2" - {
      import Day01b.getTopKCalories

      "should find the total calories that the Elf with the largest number of calories carries" in {
        getTopKCalories(calories, 3) shouldEqual 45000
      }
    }
  }

  "Day 02" - {

    "Part 1" - {
      import Day02a.{Round, Shape, calcTotalScore}

      "should calculate the total score of a rock-paper-scissors game" in {
        val rounds: List[Round] =
          List(
            Round(opponentsShape = Shape("A"), myShape = Shape("Y")),
            Round(opponentsShape = Shape("B"), myShape = Shape("X")),
            Round(opponentsShape = Shape("C"), myShape = Shape("Z")),
          )

        calcTotalScore(rounds) shouldEqual 15
      }
    }

    "Part 2" - {
      import Day02b.{Outcome, Round, Shape, getRequestedShape}

      "should return the shape with the expected outcome of the round" in {
        getRequestedShape(Shape("A"), Outcome("Y")) shouldEqual Shape.Rock
        getRequestedShape(Shape("B"), Outcome("X")) shouldEqual Shape.Rock
        getRequestedShape(Shape("C"), Outcome("Z")) shouldEqual Shape.Rock
      }
    }
  }

  "Day 03" - {

    "Part 1" - {
      import Day03a.calcSumOfPriorities

      "should calculate the sum of priorities of the common item types" in {
        val rucksackItems: List[String] =
          List(
            "vJrwpWtwJgWrhcsFMMfFFhFp",
            "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
            "PmmdzqPrVvPwwTWBwg",
            "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
            "ttgJtRGJQctTZtZT",
            "CrZsJsPPZsGzwwsLwLmpwMDw"
          )

        calcSumOfPriorities(rucksackItems) shouldEqual 157
      }
    }

    "Part 2" - {
      import Day03b.calcSumOfPriorities

      "should calculate the sum of priorities of the item types corresponding to a badge" in {
        val rucksackGroups: List[List[String]] =
          List(
            List("vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg"),
            List("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw")
          )

        calcSumOfPriorities(rucksackGroups) shouldEqual 70
      }
    }
  }

  "Day 04" - {

    "Part 1" - {
      import Day04a.{Section, calcNumberOfFullyContainedPairs}

      "should calculate the number of fully containing section pairs" in {
        val sectionPairs: List[(Section, Section)] =
          List(
            (Section(2, 4), Section(6, 8)),
            (Section(2, 3), Section(4, 5)),
            (Section(5, 7), Section(7, 9)),
            (Section(2, 8), Section(3, 7)),
            (Section(6, 6), Section(4, 6)),
            (Section(2, 6), Section(4, 8)),
          )

        calcNumberOfFullyContainedPairs(sectionPairs) shouldEqual 2
      }
    }

    "Part 2" - {
      import Day04b.{Section, calcNumberOfOverlappingPairs}

      "should calculate the number of overlapping section pairs" in {
        val sectionPairs: List[(Section, Section)] =
          List(
            (Section(2, 4), Section(6, 8)),
            (Section(2, 3), Section(4, 5)),
            (Section(5, 7), Section(7, 9)),
            (Section(2, 8), Section(3, 7)),
            (Section(6, 6), Section(4, 6)),
            (Section(2, 6), Section(4, 8)),
          )

        calcNumberOfOverlappingPairs(sectionPairs) shouldEqual 4
      }
    }
  }

end AocSuite
