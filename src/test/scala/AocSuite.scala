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

  "Day 05" - {
    "Part 1" - {
      import Day05a.{Move, getTopItems}

      "should calculate the string spelled by the crate tops" in {
        val initialCrates: Vector[List[Char]] = Vector(List('N', 'Z'), List('D', 'C', 'M'), List('P'))
        val moves: List[Move] = List(Move(2, 1, 1), Move(1, 3, 3), Move(2, 1, 2), Move(1, 2, 1))

        getTopItems(initialCrates, moves) shouldEqual "CMZ"
      }
    }

    "Part 2" - {
      import Day05b.{Move, getTopItems}

      "should calculate the string spelled by the crate tops when multiple items can be moved at once" in {
        val initialCrates: Vector[List[Char]] = Vector(List('N', 'Z'), List('D', 'C', 'M'), List('P'))
        val moves: List[Move] = List(Move(2, 1, 1), Move(1, 3, 3), Move(2, 1, 2), Move(1, 2, 1))

        getTopItems(initialCrates, moves) shouldEqual "MCD"
      }
    }
  }

  "Day 06" - {
    val signals: List[String] =
      List(
        "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
        "bvwbjplbgvbhsrlpgdmjqwftvncz",
        "nppdvjthqldpwncqszvftbrmjlhg",
        "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
        "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
      )

    "Part 1" - {
      import Day06a.findStartOfPacketMarker

      "should calculate the index of the start-of-packet marker" in {
        val markerSize: Int = 4
        signals.map(findStartOfPacketMarker(_, markerSize)) shouldEqual List(7, 5, 6, 10 ,11)
      }
    }

    "Part 2" - {
      import Day06b.findStartOfMessageMarker

      "should calculate the index of the start-of-message marker" in {
        val markerSize: Int = 14
        signals.map(findStartOfMessageMarker(_, markerSize)) shouldEqual List(19, 23, 23, 29, 26)
      }
    }
  }

  "Day 07" - {
    val terminalOutput: List[String] =
      List(
        "$ cd /",
        "$ ls",
        "dir a",
        "14848514 b.txt",
        "8504156 c.dat",
        "dir d",
        "$ cd a",
        "$ ls",
        "dir e",
        "29116 f",
        "2557 g",
        "62596 h.lst",
        "$ cd e",
        "$ ls",
        "584 i",
        "$ cd ..",
        "$ cd ..",
        "$ cd d",
        "$ ls",
        "4060174 j",
        "8033020 d.log",
        "5626152 d.ext",
        "7214296 k"
      )

    "Part 1" - {
      import Day07a.{Directory, Path, parseFileSystem, calcTotalSizeOfSmallDirectories}

      "should calculate the total size of directories whose size does not exceed 100000" in {
        val limit: Int = 100000
        val directoryStructure: Map[Path, Directory] = parseFileSystem(terminalOutput.iterator)
        calcTotalSizeOfSmallDirectories(directoryStructure, limit) shouldEqual 95437
      }
    }

    "Part 2" - {
      import Day07b.{Directory, Path, parseFileSystem, findSmallestRemovableDirectory}

      "should calculate the size of the smallest directory whose removal frees enough memory" in {
        val availableDiskSpace: Int = 70000000
        val requiredSpace: Int = 30000000
        val directoryStructure: Map[Path, Directory] = parseFileSystem(terminalOutput.iterator)
        findSmallestRemovableDirectory(directoryStructure, availableDiskSpace, requiredSpace) shouldEqual 24933642
      }
    }

    "Day 08" - {
      val trees: Vector[Vector[Int]] =
        Vector(
          Vector(3, 0, 3, 7, 3),
          Vector(2, 5, 5, 1, 2),
          Vector(6, 5, 3, 3, 2),
          Vector(3, 3, 5, 4, 9),
          Vector(3, 5, 3, 9, 0)
        )

      "Part 1" - {
        import Day08a.{TreeGrid, countNrVisibleTrees}

        "should count the number of trees visible from either side of the forest" in {
          countNrVisibleTrees(TreeGrid(5, 5, trees)) shouldEqual 21
        }
      }

      "Part 2" - {
        import Day08b.{TreeGrid, getHighestScenicScore}

        "should return the highest scenic score" in {
          getHighestScenicScore(TreeGrid(5, 5, trees)) shouldEqual 8
        }
      }
    }

    "Day 09" - {
      "Part 1" - {
        import Day09a.{Direction, Motion, calcNrVisitedPositions}

        "should calculate the number of unique positions that tail has visited" in {
          val motions: List[Motion] =
            List(
              Motion(Direction.Right, 4),
              Motion(Direction.Up, 4),
              Motion(Direction.Left, 3),
              Motion(Direction.Down, 1),
              Motion(Direction.Right, 4),
              Motion(Direction.Down, 1),
              Motion(Direction.Left, 5),
              Motion(Direction.Right, 2)
            )

          calcNrVisitedPositions(motions) shouldEqual 13
        }
      }

      "Part 2" - {
        import Day09b.{Direction, Motion, calcNrVisitedPositions}

        "should calculate the number of unique positions that the tail of a large rope has visited" in {
          val motions: List[Motion] =
            List(
              Motion(Direction.Right, 5),
              Motion(Direction.Up, 8),
              Motion(Direction.Left, 8),
              Motion(Direction.Down, 3),
              Motion(Direction.Right, 17),
              Motion(Direction.Down, 10),
              Motion(Direction.Left, 25),
              Motion(Direction.Up, 20)
            )

          val ropeSize: Int = 10
          calcNrVisitedPositions(motions, ropeSize) shouldEqual 36
        }
      }
    }

    "Day 10" - {
      val lines: List[String] = scala.io.Source.fromResource("test10.txt").getLines().toList

      "Part 1" - {
        import Day10a.{Program, calcSumOfSignalStrengths, readInstructions}

        "should calculate the signal strength" in {
          val program: List[Program] = readInstructions(lines.iterator)
          calcSumOfSignalStrengths(program) shouldEqual 13140
        }
      }

      "Part 2" - {
        import Day10b.{Pixel, Program, calcScreenContent, readInstructions}

        "should calculate the visible CRT screen pixels" in {
          val program: List[Program] = readInstructions(lines.iterator)
          val screen: List[Pixel] = calcScreenContent(program)
          val expectedContent: List[String] = List(
            "##..##..##..##..##..##..##..##..##..##..",
            "###...###...###...###...###...###...###.",
            "####....####....####....####....####....",
            "#####.....#####.....#####.....#####.....",
            "######......######......######......####",
            "#######.......#######.......#######....."
          )

          screen.grouped(40).map(_.mkString).toList shouldEqual expectedContent
        }
      }
    }

    "Day 11" - {

      "Part 1" - {
        import Day11a.{Monkey, countMonkeyBusinessLevel}

        "should calculate the level of monkey business" in {
          val monkeys: Vector[Monkey] =
            Vector(
              Monkey(List(79, 98), 0, (x: Int) => x * 19, 23, 2, 3),
              Monkey(List(54, 65, 75, 74), 1, (x: Int) => x + 6, 19, 2, 0),
              Monkey(List(79, 60, 97), 2, (x: Int) => x * x, 13, 1, 3),
              Monkey(List(74), 3, (x: Int) => x + 3, 17, 0, 1)
            )

          countMonkeyBusinessLevel(monkeys, nrRounds = 20) shouldEqual 10605
        }
      }

      "Part 2" - {
        import Day11b.{Monkey, countMonkeyBusinessLevel}

        "should calculate the level of very long monkey business" in {
          val monkeys: Vector[Monkey] =
            Vector(
              Monkey(List(79, 98), 0, (x: Long) => x * 19, 23, 2, 3),
              Monkey(List(54, 65, 75, 74), 1, (x: Long) => x + 6, 19, 2, 0),
              Monkey(List(79, 60, 97), 2, (x: Long) => x * x, 13, 1, 3),
              Monkey(List(74), 3, (x: Long) => x + 3, 17, 0, 1)
            )

          countMonkeyBusinessLevel(monkeys, nrRounds = 20) shouldEqual 10197L
        }
      }
    }
  }

end AocSuite
