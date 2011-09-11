/*

 Author: Michid
 Site:   http://michid.wordpress.com/2010/12/06/regular-expression-matching-in-100-lines-of-code/

 */

trait RegExp {
  def nullable: Boolean
  def derive(c: Char): RegExp
}

case object Empty extends RegExp {
  def nullable = false
  def derive(c: Char) = Empty
}

case object Eps extends RegExp {
  def nullable = true
  def derive(c: Char) = Empty
}

case class Str(s: String) extends RegExp {
  def nullable = s.isEmpty
  def derive(c: Char) =
    if (s.isEmpty || s.head != c) Empty
    else Str(s.tail)
}

case class Cat(r: RegExp, s: RegExp) extends RegExp {
  def nullable = r.nullable && s.nullable
  def derive(c: Char) =
    if (r.nullable) Or(Cat(r.derive(c), s), s.derive(c))
    else Cat(r.derive(c), s)
}

case class Star(r: RegExp) extends RegExp {
  def nullable = true
  def derive(c: Char) = Cat(r.derive(c), this)
}

case class Or(r: RegExp, s: RegExp) extends RegExp {
  def nullable = r.nullable || s.nullable
  def derive(c: Char) = Or(r.derive(c), s.derive(c))
}

case class And(r: RegExp, s: RegExp) extends RegExp {
  def nullable = r.nullable && s.nullable
  def derive(c: Char) = And(r.derive(c), s.derive(c))
}

case class Not(r: RegExp) extends RegExp {
  def nullable = !r.nullable
  def derive(c: Char) = Not(r.derive(c))
}
