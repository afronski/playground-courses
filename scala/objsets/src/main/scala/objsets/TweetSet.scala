package objsets

import common._
import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {

  override def toString: String =
    "User: " + user + " " +
      "Text: " + text + " [" + retweets + "]"

}

abstract class TweetSet {

  def filter(p: Tweet => Boolean): TweetSet = this.filter0(p, new Empty)
  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet =
    if (!this.isEmpty) this.union0(that, new Empty)
    else that.union0(this, new Empty)

  def union0(that: TweetSet, accu: TweetSet): TweetSet

  def ascendingByRetweet: Trending = ascendingByRetweet0(new EmptyTrending)
  def ascendingByRetweet0(accu: Trending): Trending

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def incl(x: Tweet): TweetSet
  def contains(x: Tweet): Boolean
  def isEmpty: Boolean
  def head: Tweet
  def tail: TweetSet

  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }

  def remove(tw: Tweet): TweetSet

  def findMin0(curr: Tweet): Tweet =
    if (this.isEmpty) curr
    else if (this.head.retweets < curr.retweets) this.tail.findMin0(this.head)
    else this.tail.findMin0(curr)

  def findMin: Tweet =
    this.tail.findMin0(this.head)
  // -------------------------------------------------------------------------
}

class Empty extends TweetSet {

  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = accu
  def union0(that: TweetSet, accu: TweetSet): TweetSet = accu
  def ascendingByRetweet0(accu: Trending): Trending = accu

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def contains(x: Tweet): Boolean = false
  def incl(x: Tweet): TweetSet = new NonEmpty(x, new Empty, new Empty)
  def isEmpty = true
  def head = throw new Exception("Empty.head")
  def tail = throw new Exception("Empty.tail")
  def remove(tw: Tweet): TweetSet = this
  // -------------------------------------------------------------------------

  override def toString() = "."
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet =
    left.filter0(p, right.filter0(p, if (p(elem)) accu.incl(elem) else accu))

  def union0(that: TweetSet, accu: TweetSet): TweetSet =
    right.union0(left,
      left.union0(right,
        that.union0(left,
          that.union0(right,
            if (!accu.contains(elem)) accu.incl(elem)
            else accu))))

  def ascendingByRetweet0(accum: Trending): Trending =
    this.remove(this.findMin).ascendingByRetweet0(accum + this.findMin)

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def isEmpty = false
  def head = if (left.isEmpty) elem else left.head
  def tail = if (left.isEmpty) right else new NonEmpty(elem, left.tail, right)

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)
  // -------------------------------------------------------------------------

  override def toString() = "\n{" + left + elem + right + "}"
}

abstract class Trending {
  def +(tw: Tweet): Trending
  def head: Tweet
  def tail: Trending
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }
}

class EmptyTrending extends Trending {
  def +(tw: Tweet) = new NonEmptyTrending(tw, new EmptyTrending)

  def head: Tweet = throw new Exception
  def tail: Trending = throw new Exception
  def isEmpty: Boolean = true

  override def toString = "EmptyTrending"
}

class NonEmptyTrending(elem: Tweet, next: Trending) extends Trending {
  def +(tw: Tweet): Trending =
    new NonEmptyTrending(elem, next + tw)

  def head: Tweet = elem
  def tail: Trending = next
  def isEmpty: Boolean = false

  override def toString =
    "NonEmptyTrending([" + elem.user + "] " + elem.retweets + ", " + next + ")"
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")

  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  val googleTweets: TweetSet = TweetReader.allTweets.filter(tweet => google.exists(tweet.text.contains))

  val appleTweets: TweetSet = TweetReader.allTweets.filter(tweet => apple.exists(tweet.text.contains))

  // Q: from both sets, what is the tweet with highest #retweets?
  val trending: Trending = googleTweets.union(appleTweets).ascendingByRetweet
}

object Main extends App {
  val set1 = new Empty
  val set2 = set1.incl(new Tweet("a", "a body", 20))
  val set3 = set2.incl(new Tweet("b", "b body", 20))
  val c = new Tweet("c", "c body", 7)
  val d = new Tweet("d", "d body", 9)
  val set4c = set3.incl(c)
  val set4d = set3.incl(d)
  val set5 = set4c.incl(d)

  println(set5.filter(tw => tw.user == "a"))
  println()

  println(set4c.union(set4d))
  println()

  println(set1.union(set5))
  println()

  println(set5.ascendingByRetweet)
  println()
}
