class Tests extends munit.FunSuite {
  test("1. infiniFoldRight") {
    // []
    assertEquals(Void.infiniFoldRight(0)(_ + _), 0)

    // [1]
    val l1 = Cons1(1, Void)
    assertEquals(l1.infiniFoldRight(3)(_ + _), 4)
    assertEquals(l1.infiniFoldRight(2)(_ * _), 2)

    // [1,2,3]
    val l2 = Cons1(1, Cons1(2, Cons1(3, Void)))
    assertEquals(l2.infiniFoldRight(0)(_ + _), 6)
    assertEquals(l2.infiniFoldRight(1)(_ * _), 6)

    // [[1,2,3],4,5]
    val l3 = Cons2(l2, Cons1(4, Cons1(5, Void)))
    assertEquals(l3.infiniFoldRight("end")(_.toString + " " + _), "1 2 3 4 5 end")

    // [1,2,3,[],[[4,5,6]]]
    val l4 = Cons1(1, Cons1(2, Cons1(3, Cons2(Void, Cons2(Cons2(Cons1(4, Cons1(5, Cons1(6, Void))), Void), Void)))))
    assertEquals(l4.infiniFoldRight(0)(_ + _), 21)
    assertEquals(l4.infiniFoldRight("end")(_.toString + " " + _), "1 2 3 4 5 6 end")
  }

  test("2. count") {
    // []
    assertEquals(Exam.count(Void), 0)
    // [1]
    assertEquals(Exam.count(Cons1(1, Void)), 1)
    // [1,2,3]
    assertEquals(Exam.count(Cons1(1, Cons1(2, Cons1(3, Void)))), 3)
    // [[]]
    assertEquals(Exam.count(Cons2(Void, Void)), 0)
    // [[1,2,3],4,5]
    assertEquals(Exam.count(Cons2(Cons1(1, Cons1(2, Cons1(3, Void))), Cons1(4, Cons1(5, Void)))), 5)
  }

  test("3. flatten") {
    // []
    assertEquals(Exam.flatten(Void), Nil)
    // [1]
    assertEquals(Exam.flatten(Cons1(1, Void)), List(1))
    // [1,2,3]
    assertEquals(Exam.flatten(Cons1(1, Cons1(2, Cons1(3, Void)))), List(1, 2, 3))
    // [[]]
    assertEquals(Exam.flatten(Cons2(Void, Void)), Nil)
    // [[1,2,3],4,5]
    assertEquals(Exam.flatten(Cons2(Cons1(1, Cons1(2, Cons1(3, Void))), Cons1(4, Cons1(5, Void)))), List(1, 2, 3, 4, 5))

  }

  test("4. firstChar") {
    // this function reorders the output of firstchar to be sorted (both the outer list and the inner lists)
    def reorder(l: List[(Char, List[String])]): List[(Char, List[String])] = l.map(p => (p._1, p._2.sorted)).sortBy(_._1)

    val l1 = List(
      "wood",
      "well",
      "wheel",
      "white",
      "lodge",
      "lights",
      "stag",
      "river",
      "spider"
    )
    val l1_grouped = List(
      ('w', List("wood", "well", "wheel", "white")),
      ('l', List("lodge", "lights")),
      ('s', List("stag", "spider")),
      ('r', List("river"))
    )

    assertEquals(reorder(Exam.firstChar(l1)), reorder(l1_grouped))


    val l2 = List(
      "Slee",
      "Violet",
      "Saliba",
      "Victor",
      "Clifton",
      "Sylvia",
      "Enid",
      "Valciane",
      "Elridge",
      "Cat Caro"
    )

    val l2_grouped = List(
      ('S', List("Slee", "Saliba", "Sylvia")),
      ('V', List("Violet", "Victor", "Valciane")),
      ('C', List("Clifton", "Cat Caro")),
      ('E', List("Enid", "Elridge"))
    )

    assertEquals(reorder(Exam.firstChar(l2)), reorder(l2_grouped))

  }

  test("5. rot") {
    assertEquals(Exam.rot(1, List(1, 2, 3)), List(2, 3, 1))
    assertEquals(Exam.rot(3, (1 to 6).toList), List(4, 5, 6, 1, 2, 3))
    assertEquals(Exam.rot(3, List(1, 2, 3)), List(1, 2, 3)) // full rotation
    assertEquals(Exam.rot(9, (1 to 6).toList), List(4, 5, 6, 1, 2, 3)) // 3 more than full rotation
  }

  test("6. crop") {
    val img1 = List(
      List(1,2,3,4),
      List(5,6,7,8),
      List(9,1,2,3),
      List(4,5,6,7)
    )
    val mask1 = List(
      List(0,0,0,0),
      List(0,1,1,0),
      List(0,1,1,0),
      List(0,0,0,0)
    )
    val rez1 = List(
      List(6,7),
      List(1,2)
    )

    assertEquals(Exam.crop(img1,mask1), rez1)

    val img2 = List(
      List(0,1),
      List(1,2)
    )
    val mask2 = List(
      List(1,0),
      List(1,0)
    )
    val rez2 = List(
      List(0),
      List(1)
    )

    assertEquals(Exam.crop(img2, mask2), rez2)
  }
}