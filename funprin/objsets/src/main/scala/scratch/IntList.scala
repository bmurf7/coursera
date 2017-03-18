package scratch 

trait IntList {
  def head: Int
  def tail: IntList
  def isEmpty: Boolean
  def foreach(f: Int => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends IntList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Int, val tail: IntList) extends IntList {
  def isEmpty = false

  def reverse: IntList = {
    def iter(fr: IntList, to: IntList): IntList = {
      if (fr.isEmpty) to
      else {
        println(fr.head)
        iter(fr.tail, new Cons(fr.head, to))
      }
    }
    iter(this, Nil)
  }
}

/*
object Main extends App {
  println("IntList")

  val tl = new Cons(1, new Cons(2, new Cons(3, Nil)))
  println("before " + tl.head)
  val after = tl.reverse
  println("after " + after.head)
}
*/
