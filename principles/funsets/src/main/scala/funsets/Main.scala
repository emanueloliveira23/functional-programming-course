package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))


  val odd = (x: Int) => (x % 2) != 0
  val doubleOdd = map(odd, _ * 2)
  printSet(doubleOdd)
}
