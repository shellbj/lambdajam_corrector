package jam

object corrector {

  type Word = String

  type Correction = List[Word]

  type SplitWord = List[(Word, Word)]

  val alphabet = "abcdefghijklmnopqrstuvwxyz"

  def edits1(w: Word): Set[Word] = {
    val s = toTuples(w)
    Set(deletes(s) :: transposed(s) :: replaces(s) :: inserts(s))

    def toTuples(w: Word): SplitWord =
      for {
        i <- 0 to w.length
      } yield w.splitAt(i)


    def deletes(s: SplitWord): Correction = {
      for {
        (a, b) <- s
        if b != None
        if !b.isEmpty
      } yield a ++ b.tail
    }

    def transposed(s: SplitWord): Correction = {
      for {
        (a, b) <- s
        if b.length > 1
      } yield (((a ++ b.tail.head) ++ b.head) ++ b.drop(2))
    }

    def replaces(s: SplitWord): Correction = {
      for {
        (a, b) <- s
        if b != None
        if !b.isEmpty
        c <- alphabet
      } yield  a :: c :: b.tail
    }

    def inserts(s: SplitWord): Correction = {
      for {
        (a, b) <- s
        c <- alphabet
      } yield  a ++ c ++ b
    }
  }
}
