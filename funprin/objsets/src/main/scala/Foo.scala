/**
  * Created by murphbt on 7/14/16.
  */
import objsets._

object Foo { //extends App {

  val txt = "The quick brown fox jumps over the fence."
  val words = txt.split(" ")
  words.map(_.trim)
  //for (w <- words) println(w)

  //val found = GoogleVsApple.bar(new Tweet("btm","love my ipad",13))
  //println(found)

}
