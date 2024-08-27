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


  @tailrec
  def evalPacket(p: Packet, acl: ACL): Boolean = acl match {
    case Nil => true
    case x :: xs => x(p) && evalPacket(p, xs)
  }


  /*
  * EXERCITIUL 1
  * Scrieti o funtie care primeste o secventa de pachete `p1, ... pn` si un ACL si
  * returneaza o subsecventa (posibil vida) de pachete `p1, ... pk-1` astfel incat `pk`
  * este primul pachet din secventa care este respins de ACL, cu 1 <= k <= n.
  */
  def acceptSequence(packets: List[Packet], acl: ACL): List[Packet] = {
    @tailrec
    def helper(packs: List[Packet], acc: List[Packet]): List[Packet] = packs match {
      case Nil => acc
      case x :: xs => {
        if (evalPacket(x, acl) == false) acc
        else helper(xs, acc :+ x)
      }
    }

    helper(packets, List.empty)
  }



  /*
  * EXERCITIUL 2
  * Scrieti o functie care primeste un ACL si transforma lista de predicate intr-un singur predicat.
  * Mai precis, creati un predicat care returneaza true pentru fiecare pachet permis de ACL, si false,
  * in caz contrar. Hint: folositi fold.
  */
  def foldACL(acl: ACL): Packet => Boolean = {  // functia intoarce o `expresie lambda`
    val all_true: (Packet => Boolean) = (p: Packet) => true        // expresie lambda (primeste packet, intoarce bool)
    val all_false: (Packet => Boolean) = (p: Packet) => false      // expresie lambda (primeste packet, intoarce bool)
    acl.foldLeft(all_true)((acc: (Packet => Boolean), value: (Packet => Boolean)) => pack => acc(pack) && value(pack))   // functia intoarce o expresie lambda
  }


  /*
  * EXERCITIUL 3
  * Scrieti o functie care primeste doua ACL-uri si verifica daca sunt echivalente peste setul
  * de pachete UDP cu porturi intre 0 si 10.
  * Doua ACL-uri sunt echivalente daca au aceeasi comportament (accepta sau resping aceleasi pachete).
  */
  def equivalentUDP10(acl1: ACL, acl2: ACL): Boolean = {
    @tailrec
    def helper(port: Int): Boolean = {
      if (port > 10) true
      else if (evalPacket(UDP(port), acl1) != evalPacket(UDP(port), acl2)) false
      else helper(port + 1)
    }

    helper(0)
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

    def getProductPrice(prodname: ProductName, inventoryList: List[Product]): Price = inventoryList match {
      case Nil => -1 // eroare
      case x :: xs => {
        if (x._1 == prodname) x._2
        else getProductPrice(prodname, xs)
      }
    }

    def getCostumerSum(itemsList: List[ProductName]): Price = itemsList match {
      case Nil => 0
      case x :: xs => getProductPrice(x, productInventory) + getCostumerSum(xs)
    }

    def helper(shopLists: List[ShoppingList], clientsumList: List[(CustomerName, Price)]): List[(CustomerName, Price)] = shopLists match {
      case Nil => clientsumList
      case x :: xs => {
        val customername = x._1
        val items = x._2
        helper(xs, clientsumList :+ (customername, getCostumerSum(items)))
      }
    }

    helper(shoppingLists, Nil)
  }
  /*
  * EXERCITIUL 5
  * Scrieti o functie care primeste o lista de valori intregi "boxed" intr-un container de tip Option:
  * un "container" este fie gol, fie contine un intreg.
  * Functia ar trebui sa returneze o lista "boxed" de intregi, daca TOATE "containerele" care nu sunt goale
  * contin un intreg strict pozitiv, si None, in caz contrar.
  */
  def positiveValues(l: List[Option[Int]]): Option[List[Int]] = {
    val nums: List[Int] = l.filter(el => el match
      case Some(x) => true
      case None => false
    ).map(x => x.getOrElse(-1))

    val pos: List[Int] = nums.filter(x => x > 0)
    val contPos: Option[List[Int]] = Some(nums)

    if (nums.length != pos.length) None
    else Some(pos)
  }

}