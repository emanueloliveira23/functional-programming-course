object iterator {

  trait Iterator[T] {
    def hasNext: Boolean
    def next(): T
    def foldLeft[S](z: S)(f: (S, T) => S): S = {
      var result = z
      while (hasNext) {
        result = f(result, next())
      }
      result
    }
  }

  class EvenIterator(n: Int) extends Iterator[Int] {
    private var i = 0
    def hasNext: Boolean = i <= n
    def next(): Int = {
      i += 2
      i
    }
  }

  var ei = new EvenIterator(2)
  ei.foldLeft(1)(_ + _)
}