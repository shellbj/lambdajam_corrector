package jam

import scala.util.matching.Regex

object corrector {

  type Word = String

  type Correction = List[Word]

  type SplitWord = List[(Word, Word)]

  val word = """[a-z]+""".r
  val source = scala.io.Source.fromFile("/home/evan/lambda/spelling-jam/data/test1.txt")
  val text = source.mkString
  source.close()

  def train(words:Iterable[String]) : Map[String,Int] = {
    words.foldLeft(Map[String,Int] () withDefaultValue 0){
      case (wordCount,inWord) =>
        val lInWord = inWord.toLowerCase
        wordCount + (lInWord -> (1 + wordCount(lInWord)))
    }
  }

  val NWORDS = train(word.findAllIn(text).toIterable)

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

  def known_edits2(word: String) : Set[String] = {
    val edits = edits1(word)
    val edits2 = edits.flatMap(edits1(_))
      (edits ++ edits2).filter(NWORDS.contains(_))
  }

}
