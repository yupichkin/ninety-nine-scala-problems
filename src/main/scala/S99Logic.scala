class S99Logic(val b: Boolean) {
  // P47
  def and(y: Boolean): Boolean = ???

  def or(y: Boolean): Boolean = ???

  def nand(y: Boolean): Boolean = ???

  def nor(y: Boolean): Boolean = ???

  def xor(y: Boolean): Boolean = ???

  def impl(y: Boolean): Boolean = ???

  def equ(y: Boolean): Boolean = ???
}

object S99Logic {
  implicit def boolean2S99Logic(b: Boolean): S99Logic = new S99Logic(b)

  // P46
  def and(x: Boolean, y: Boolean): Boolean = ???

  def or(x: Boolean, y: Boolean): Boolean = ???

  def nand(x: Boolean, y: Boolean): Boolean = ???

  def nor(x: Boolean, y: Boolean): Boolean = ???

  def xor(x: Boolean, y: Boolean): Boolean = ???

  def impl(x: Boolean, y: Boolean): Boolean = ???

  def equ(x: Boolean, y: Boolean): Boolean = ???

  def table2(f: (Boolean, Boolean) => Boolean): Map[(Boolean, Boolean), Boolean] = ???

  // P47
  def not(x: Boolean): Boolean = ???

  // P49
  def gray(x: Int): List[String] = ???

  // P50
  def huffman(symbolFrequencies: List[(Symbol, Int)]): List[(Symbol, String)] = ???
}
