package jam

import scala.util.matching.Regex

object corrector {
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

  def edits1(w: String): Set[String] = {
    def toTuples(w: String): Seq[(String, String)] =
      for {
        i <- 0 to w.length
      } yield w.splitAt(i)


    def deletes(s: Seq[(String, String)]): Seq[String] = {
      for {
        (a, b) <- s
        if b != None
        if !b.isEmpty
      } yield a ++ b.tail
    }

    def transposed(s: Seq[(String, String)]): Seq[String] = {
      s.map({case (a,b) => a + b(1) + b(0) + b.drop(2)})
    }

    def replaces(s: Seq[(String, String)]): Seq[String] = {
      alphabet.flatMap({case c => s.map({case (a,b) => a + c + b.tail})})
    }

    def inserts(s: Seq[(String, String)]): Seq[String] = {
      alphabet.flatMap({case c => s.map({case (a,b) => a + c + b})})
    }

    val s = toTuples(w)
    Set() ++ deletes(s) ++ transposed(s) ++ replaces(s) ++ inserts(s)
  }

  def known_edits2(word: String) : Set[String] = {
    val edits2 = edits1(word).flatMap(edits1(_))
    edits2.filter(NWORDS.contains(_))
  }

  def known(words: Set[String]): Set[String] = {
    words.filter(NWORDS.contains(_))
  }

  def correct(word: String): List[String] = {
    val candidates = known(Set(word)) ++ known(edits1(word)) ++ known_edits2(word) ++ Set(word)
    candidates.foldLeft(Map[String,Int] () withDefaultValue 0) {
      case (wordCount,inWord) => wordCount + (inWord->NWORDS(inWord))}.toList.sortBy({case (k,v) => -v }).map({case (k,v) => k})
  }
}
