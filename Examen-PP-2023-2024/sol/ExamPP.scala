import scala.annotation.tailrec

object ExamPP {

  def profileID = 111226 // Aici va trebui sa puneti ID-ul vostru de pe moodle

  /*
  * Vom defini TDA-ul `Packet` care va avea foi constructori `UDP` si `TCP`.
  */
  trait Packet
  case class UDP(port: Int) extends Packet
  case class TCP(port: Int) extends Packet

  /*
  * Definim un ACL (Access Control List) ca fiind o lista de predicate.
  * Un pachet este acceptat de un ACL daca toate predicatele din lista sunt adevarate, altfel este respins.
  */
  type ACL = List[Packet => Boolean]

  def evaluatePacket(packet: Packet, acl: ACL): Boolean = acl match {
    case Nil => true // toate predicatele au fost trecute
    case func :: otherFuncs => {
      if (func(packet) == false) false // pachetul nu a trecut un predicat (este respins)
      else evaluatePacket(packet, otherFuncs)
    }
  }
  /*
  * EXERCITIUL 1
  * Scrieti o funtie care primeste o secventa de pachete `p1, ... pn` si un ACL si
  * returneaza o subsecventa (posibil vida) de pachete `p1, ... pk-1` astfel incat `pk`
  * este primul pachet din secventa care este respins de ACL, cu 1 <= k <= n.
  */
  def acceptSequence(packets: List[Packet], acl: ACL): List[Packet] = {
    @tailrec def helper(l: List[Packet], acc: List[Packet]): List[Packet] = l match {
      case Nil => acc
      case x :: xs => {
        if (evaluatePacket(x, acl) == false) acc
        else helper(xs, acc :+ x)
      }
    }

    helper(packets, Nil)
  }

  def respingeOricePacket(p: Packet): Boolean = false


  /*
  * EXERCITIUL 2
  * Scrieti o functie care primeste un ACL si transforma lista de predicate intr-un singur predicat.
  * Mai precis, creati un predicat care returneaza true pentru fiecare pachet permis de ACL, si false,
  * in caz contrar. Hint: folositi fold.
  */
  def foldACL(acl: ACL): Packet => Boolean = {
    acl.foldLeft((p: Packet) => true)((acc: Packet => Boolean, value: Packet => Boolean) =>
      (p: Packet) => acc(p) && value(p))
  }
  /*
  * EXERCITIUL 3
  * Scrieti o functie care primeste doua ACL-uri si verifica daca sunt echivalente peste setul
  * de pachete UDP cu porturi intre 0 si 10.
  * Doua ACL-uri sunt echivalente daca au aceeasi comportament (accepta sau resping aceleasi pachete).
  */
  def equivalentUDP10(acl1: ACL, acl2: ACL): Boolean = {
    def helper(pred1: ACL, pred2: ACL, idx: Int): Boolean = {
      if (idx > 10) true
      else (evaluatePacket(UDP(idx), pred1) == evaluatePacket(UDP(idx), pred2)) && helper(pred1, pred2, idx + 1)
    }

    helper(acl1, acl2, 0)
  }

  /*
  * Vom defini tipul `Product` ca avand un nume si un pret.
  * Un `ShoppingList` este o pereche formata din numele unui client si o lista de nume de produse
  * pe care acesta doreste sa le cumpere.
  */

  type Price = Float
  type ProductName = String
  type Product = (ProductName, Price)
  type CustomerName = String
  type ShoppingList = (CustomerName, List[ProductName])

  /*
  * EXERCITIUL 4
  * Se considera ca pretul listei de cumparaturi a unui client este suma preturilor produselor din lista sa.
  * Pentru mai multe liste de cumparaturi, calculati pretul fiecareia.
  */
  def shoppingListPrices(shoppingLists: List[ShoppingList], productInventory: List[Product]): List[(CustomerName, Price)] = {

    def getPriceofProduct(name: ProductName): Price = {
      @tailrec def helper(products: List[Product]): Price = products match {
        case Nil => 0.0 // nu contine produsul
        case x :: xs =>
          if (x._1 == name) x._2
          else helper(xs)
      }

      helper(productInventory)
    }

    shoppingLists.map { case (name, shoppinglist) =>
      (name, shoppinglist.foldLeft(0.0.toFloat)((acc: Price, value: ProductName) => acc + getPriceofProduct(value)))
    }
  }
  /*
  * EXERCITIUL 5
  * Scrieti o functie care primeste o lista de valori intregi "boxed" intr-un container de tip Option:
  * un "container" este fie gol, fie contine un intreg.
  * Functia ar trebui sa returneze o lista "boxed" de intregi, daca TOATE "containerele" care nu sunt goale
  * contin un intreg strict pozitiv, si None, in caz contrar.
  */
  def positiveValues(l: List[Option[Int]]): Option[List[Int]] = {
    val els: List[Int] = l.filter(x => x match
      // eliminam toate containerele None din lista
      case None => false
      case Some(x) => true
    ).map(x => x.get)

    val pozitives: List[Int] = els.filter(x => x > 0)
    val negatives: List[Int] = els.filterNot(x => x > 0)

    if (!negatives.isEmpty) None
    else Some(pozitives)
  }

}