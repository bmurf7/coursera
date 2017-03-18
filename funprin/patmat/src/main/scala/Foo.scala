/**
  * Created by murphbt on 7/18/16.
  */
import patmat.Huffman._

object Foo extends App {

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)
  val decoded = decode(frenchCode, secret)
  println(s"decoded=$decoded")

  //println(s"frenchCode=$frenchCode")
  //println(frenchCode)

  val french: List[(Char,Int)] = List(('s',121895),('d',56269),('x',5928),('j',8351),('f',16351),('z',2093),('k',745),('w',1747),('y',4725),('h',11298),('q',20889),('o',82762),('l',83668),('m',45521),('p',46335),('u',96785),('r',100500),('c',50003),('v',24975),('g',13288),('b',13822),('n',108812),('t',111103),('e',225947),('i',115465),('a',117110))
  println(french.size + " Leafs in frenchCode")
  /*
  val ordered = makeOrderedLeafList(french)
  val codeTree = until(singleton,combine)(ordered).head

  //println(s"french=$french")
  //println(s"ordered=$ordered")

  println(s"          =$codeTree")
  if (frenchCode == codeTree) {
    println("equal")
  } else {
    println("NOT equal")
  }
  */

    /*
  //val txt = "the quick brown fox jumped over the lazy dog"
  val txt = "aaaaaaaabbbcdefgh"
  //val txt = "abacbacdbeacfdebacdbacbaba"
  val lst = string2Chars(txt)
  val unordered = times(lst)

  val ordered = makeOrderedLeafList(unordered)
  //println(s"unordered=$unordered")
  println(s"ordered=$ordered")

  val combine1 = combine(ordered)
  println(s"combine1=$combine1")
  val combine2 = combine(combine1)
  println(s"combine2=$combine2")
  val combine3 = combine(combine2)
  println(s"combine3=$combine3")
  val combine4 = combine(combine3)
  println(s"combine4=$combine4")

  val t1 = createCodeTree(string2Chars(txt))
  println(s"CodeTree=$t1")

  val chars = List('a','b','c','d','e','f','g','h')
  for (c <- chars) {
    val coded = encode(t1)(List[Char](c))
    println(s"$c $coded")
  }
  */

  /*
  def show(o: List[CodeTree]): String = o match {
    case List() => ""
    case l: Leaf => l.char + " " + l.weight
    case f: Fork => show(f.left) + "." + show(f.right)
  }
  */

}
