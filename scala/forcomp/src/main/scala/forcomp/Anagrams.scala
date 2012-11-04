package forcomp

import common._

object Anagrams {

  type Word = String

  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences =
    ((w.toLowerCase.toList groupBy (identity)) map (pair => (pair._1, pair._2.length))).toList sortBy (identity)

  def sentenceOccurrences(sentence: Sentence): Occurrences =
    wordOccurrences(sentence mkString)

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary groupBy wordOccurrences

  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences(wordOccurrences(word))

  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def pack(xs: List[Char]): List[List[Char]] = xs match {
      case Nil => Nil
      case x :: xs1 => {
        val (first, rest) = xs span (y => y == x)
        first :: pack(rest)
      }
    }

    val (chars, frequencies) = (occurrences map { case (char, frequency) => ((for (i <- 1 to frequency) yield char) mkString, frequency) } unzip)
    List() :: ((for (i <- 1 to frequencies.sum) yield chars.mkString.combinations(i).toList) flatMap (element => element) map (string => pack(string.toList) map (ys => (ys.head, ys.length)))).toList
  }

  def subtract(x: Occurrences, y: Occurrences): Occurrences =
    y.foldLeft(x.toMap)((a, b) => a.updated(b._1, a.getOrElse(b._1, 0) - b._2)).toList filter (pair => pair._2 > 0) sortBy (pair => pair._1)

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def innerSentenceAnagrams(occurrences: Occurrences): List[Sentence] = {
      if (occurrences.isEmpty) List(Nil)
      else for {
          combination <- combinations(occurrences)
          word <- dictionaryByOccurrences.getOrElse(combination, Nil)
          rest <- innerSentenceAnagrams(subtract(occurrences, combination))
        } yield word :: rest
    }

    innerSentenceAnagrams(sentenceOccurrences(sentence))
  } 

}
