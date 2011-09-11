/*

 Author: Michid
 Site:   http://michid.wordpress.com/2010/12/06/regular-expression-matching-in-100-lines-of-code/

 */

object Test {
  import Pimps._

  val digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
  val int = ("+" | "-").? ++ digit.%(1)
  val real = ("+" | "-").? ++ digit.%(1) ++ ("." ++ digit.%(1)).? ++ (("e" | "E") ++ ("+" | "-").? ++ digit.%(1)).?

  def main(args: Array[String]) {
    val ints = List("0", "-4534", "+049", "99")
    val reals = List("0.9", "-12.8", "+91.0", "9e12", "+9.21E-12", "-512E+01")
    val errs = List("", "-", "+", "+-1", "-+2", "2-")

    ints.foreach(s => assert(int ~ s))
    reals.foreach(s => assert(!(int ~ s)))
    errs.foreach(s => assert(!(int ~ s)))

    ints.foreach(s => assert(real ~ s))
    reals.foreach(s => assert(real ~ s))
    errs.foreach(s => assert(!(real ~ s)))
}
