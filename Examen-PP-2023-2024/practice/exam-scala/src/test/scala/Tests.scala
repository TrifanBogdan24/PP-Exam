import ExamPP.*
import org.scalatest.funsuite.AnyFunSuite

class Tests extends AnyFunSuite {

  test("Valid profile id:" + profileID) {
    assert(profileID > 0)
  }

  test("[Ex1] (1p):") {
    val acl1: ACL = List(
      {
        case UDP(port) => port < 3
        case _ => true
      },
      {
        case TCP(port) => port % 2 == 0
        case _ => true
      }
    )
    val packets1: List[Packet] = List(UDP(1), TCP(2), UDP(4), TCP(5), UDP(6))
    assert(acceptSequence(packets1, acl1) == List(UDP(1), TCP(2)))

    val acl2: ACL = List(
      {
        case UDP(port) => port < 5
        case _ => true
      },
      {
        case TCP(port) => port % 2 == 0
        case _ => true
      },
      {
        case UDP(port) => port % 2 == 1
        case _ => true
      }
    )
    val packets2: List[Packet] = List(UDP(1), TCP(2), UDP(3), TCP(5), UDP(6))
    assert(acceptSequence(packets2, acl2) == List(UDP(1), TCP(2), UDP(3)))
  }

  test("[Ex2] (1p):") {
    val acl1: ACL = List(
      {
        case UDP(port) => port < 3
        case _ => true
      },
      {
        case TCP(port) => port % 2 == 0
        case _ => true
      }
    )
    val acl2 = foldACL(acl1)

    assert(acl2(UDP(1)))
    assert(acl2(UDP(2)))
    assert(!acl2(UDP(3)))
    assert(!acl2(UDP(4)))
    assert(!acl2(UDP(5)))
    assert(!acl2(UDP(6)))
    assert(!acl2(UDP(7)))
    assert(!acl2(UDP(8)))
    assert(!acl2(UDP(9)))
    assert(!acl2(UDP(10)))
    assert(!acl2(TCP(1)))
    assert(acl2(TCP(2)))
    assert(!acl2(TCP(3)))
    assert(acl2(TCP(4)))
    assert(!acl2(TCP(5)))
    assert(acl2(TCP(6)))
    assert(!acl2(TCP(7)))
    assert(acl2(TCP(8)))
    assert(!acl2(TCP(9)))
    assert(acl2(TCP(10)))

    val acl3: ACL = List(
      {
        case UDP(_) => true
        case _ => false
      },
      {
        case UDP(port) => port % 3 == 1
        case _ => true
      }
    )
    val acl4 = foldACL(acl3)

    assert(acl4(UDP(1)))
    assert(!acl4(UDP(2)))
    assert(!acl4(UDP(3)))
    assert(acl4(UDP(4)))
    assert(!acl4(UDP(5)))
    assert(!acl4(UDP(6)))
    assert(acl4(UDP(7)))
    assert(!acl4(UDP(8)))
    assert(!acl4(UDP(9)))
    assert(acl4(UDP(10)))
    assert(!acl4(TCP(1)))
    assert(!acl4(TCP(2)))
    assert(!acl4(TCP(3)))
    assert(!acl4(TCP(4)))
    assert(!acl4(TCP(5)))
    assert(!acl4(TCP(6)))
    assert(!acl4(TCP(7)))
    assert(!acl4(TCP(8)))
    assert(!acl4(TCP(9)))
    assert(!acl4(TCP(10)))
  }

  test("[Ex3] (1p):") {
    val acl1: ACL = List(
      {
        case UDP(port) => port < 3
        case _ => true
      },
      {
        case TCP(port) => port % 2 == 0
        case _ => true
      },
      {
        case UDP(port) => port % 2 == 1
        case _ => true
      }
    )
    val acl2: ACL = List(
      {
        case UDP(port) => port == 1
        case _ => true
      },
      {
        case TCP(port) => List(0, 2, 4, 6, 8, 10).contains(port)
        case _ => true
      }
    )
    val acl3: ACL = List(
      {
        case UDP(port) => port < 6
        case _ => true
      },
      {
        case TCP(port) => port * 2 < 6
        case _ => true
      }
    )
    assert(equivalentUDP10(acl1, acl2))
    assert(!equivalentUDP10(acl1, acl3))
    assert(!equivalentUDP10(acl2, acl3))
  }

  test("[Ex4] (1p):") {
    val shoppingList: List[Product] = List(
      ("mere", 2.5f),
      ("pere", 3.0f),
      ("banane", 4.0f),
      ("carne", 10.0f),
      ("paine", 1.0f),
      ("oua", 5.0f),
      ("lapte", 2.0f),
      ("ciocolata", 3.5f),
      ("apa", 1.5f),
      ("suc", 2.0f)
    )
    val customers: List[ShoppingList] = List(
      ("Ion", List("mere", "pere", "banane", "carne", "paine")),
      ("Maria", List("mere", "pere", "oua", "lapte", "ciocolata")),
      ("Vasile", List("mere", "pere", "apa", "suc"))
    )
    assert(shoppingListPrices(customers, shoppingList) == List(
      ("Ion", 20.5f),
      ("Maria", 16.0f),
      ("Vasile", 9.0f)
    ))
  }

  test("[Ex5] (1p):") {
    assert(positiveValues(List(Some(11), Some(22), Some(32), Some(42))) == Some(List(11, 22, 32, 42)))
    assert(positiveValues(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
    assert(positiveValues(List(Some(1), Some(-2), Some(3))) == None)
    assert(positiveValues(List(Some(1), Some(2), None)) == Some(List(1, 2)))
    assert(positiveValues(List(None, Some(2), Some(3))) == Some(List(2, 3)))
    assert(positiveValues(List(None, None, None)) == Some(List()))
  }
}