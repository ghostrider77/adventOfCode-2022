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

end AocSuite
