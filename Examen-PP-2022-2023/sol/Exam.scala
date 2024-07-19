import scala.annotation.tailrec

/*
   60p - Scala   - 6 exercitii 10p - 3 usoare, 2 medii, 1 greu
   60p - Haskell - 6 exercitii 10p

  100p - 10 exercitii
 */

trait InfiniList {
  /*
      1. [Greu]
      Implementati functia foldRight peste InfiniListe
      Alegeti logica cea mai apropiata de functionarea foldRight pe liste conventionale
   */
  def infiniFoldRight[B](acc: B)(op: (Int, B) => B): B

}
case object Void extends InfiniList{
  def infiniFoldRight[B](acc: B)(op: (Int, B) => B): B = acc
}
case class Cons1(x: Int, xs: InfiniList) extends InfiniList {
  def infiniFoldRight[B](acc: B)(op: (Int, B) => B): B = {
    val newAcc = xs.infiniFoldRight(acc)(op)
    op(x, newAcc)
  }
}
case class Cons2(x: InfiniList, xs: InfiniList) extends InfiniList{
  def infiniFoldRight[B](acc: B)(op: (Int, B) => B): B = {
    val newAcc = xs.infiniFoldRight(acc)(op)
    x.infiniFoldRight(newAcc)(op)
  }
}
/*
Cons2( Cons1(1, Cons1(2, Void)), Cons1(3, Void)) = ((1,2),3)
 */

object Exam {
  /*
      2. [Usor] Scrieti o functie care determina numarul de intregi dintr-un InfiniList
   */
  def count(l: InfiniList): Int =
    l match {
      case Void => 0
      case Cons1(_, xs) => 1 + count(xs)
      case Cons2(l1, l2) => count(l1) + count(l2)
    }

  /*
      3. [Usor] Scrieti o functie care aplatizeaza un InfiniList:
      ((1,2),3) va deveni (1,2,3).

   */
  def flatten(l: InfiniList): List[Int] =
    l match {
      case Void => Nil
      case Cons1(x, xs) => x :: flatten(xs)
      case Cons2(l1, l2) => flatten(l1) ++ flatten(l2)
    }

  /*
      4. [Usor]
      Scrieti o functie care primeste o lista de siruri de caractere, si intoarce
      o lista de perechi (x,y) unde x este un caracter, iar y este o lista
      nevida de siruri care incep cu caracterul x. Ordonarea rezultatului nu este
      importanta (testele vor sorta listele rezultate)
   */

  def firstChar(l: List[String]): List[(Char,List[String])] = {
    //facem o lista de caractere cu primul caracter
    val firstChars = l.map(word => word.toList.head).distinct
    val res = firstChars.map(char => (char, l.filter(string => string.toList.head == char)))
    res
  }

  /*
      5. [Usor]
      Implementati o functie care realizeaza un numar de k rotatii la stanga asupra unei liste.
      rot 1 (1,2,3) = (2,3,1)
      rot 3 (1,2,3,4,5,6) = (4,5,6,1,2,3)
   */

  def rot(k: Int, l: List[Int]): List[Int] = {
    def oneRot(l: List[Int]): List[Int] =
     l.drop(1) ++ l.take(1)
    @tailrec def auxRot(k: Int, aux: List[Int]): List[Int] =
      if (k == 0) aux
      else auxRot(k - 1, oneRot(aux))

    auxRot(k, l)
  }

  /*
      6. [Mediu]
      Implementati o functie care primeste o imagine reprezentata ca o lista de liste
      de int cu valori intre 1 si 255, si o masca reprezentata ca o lista de liste de valori
      0,1. Este garantat faptul ca valorile 1 dintr-o masca descriu un dreptunghi.
      De asemenea, este garanta faptul ca imaginea si masca au aceleasi dimensiuni, si ca
      acestea sunt nevide (dar lungimea nu este in mod necesar egala cu latimea).

      Functia va realiza decuparea imaginii initiale conform mastii, iar liniile si coloanele
      ce sunt in afara mastii vor fi sterse.

        4 x 4      4 x 4
      1 2 3 4    0 0 0 0     2 x 2
      5 6 7 8    0 1 1 0    =  6 7
      9 1 2 3    0 1 1 0       1 2
      4 5 6 7    0 0 0 0

   */
  def crop(l: List[List[Int]], mask: List[List[Int]]): List[List[Int]] = {
    val croppedRows = l.zip(mask).collect { case (row, maskRow) if maskRow.contains(1) => row}
    val croppedColumns = croppedRows.transpose.zip(mask.transpose).collect { case (col, maskCol) if maskCol.contains(1) => col}
    croppedColumns.transpose
  }
}
