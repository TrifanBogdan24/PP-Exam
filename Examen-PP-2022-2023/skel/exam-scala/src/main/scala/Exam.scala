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


object Exam {
  /*
      2. [Usor] Scrieti o functie care determina numarul de intregi dintr-un InfiniList
   */
  def count(l: InfiniList): Int = ???

  /*
      3. [Usor] Scrieti o functie care aplatizeaza un InfiniList:
      ((1,2),3) va deveni (1,2,3).

   */
  def flatten(l: InfiniList): List[Int] = ???

  /*
      4. [Usor]
      Scrieti o functie care primeste o lista de siruri de caractere, si intoarce
      o lista de perechi (x,y) unde x este un caracter, iar y este o lista
      nevida de siruri care incep cu caracterul x. Ordonarea rezultatului nu este
      importanta (testele vor sorta listele rezultate)
   */

  def firstChar(l: List[String]): List[(Char,List[String])] = ???

  /*
      5. [Usor]
      Implementati o functie care realizeaza un numar de k rotatii la stanga asupra unei liste.
      rot 1 (1,2,3) = (2,3,1)
      rot 3 (1,2,3,4,5,6) = (4,5,6,1,2,3)
   */

  def rot(k: Int, l: List[Int]): List[Int] = ???

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
  def crop(l: List[List[Int]], mask: List[List[Int]]): List[List[Int]] = ???
}
