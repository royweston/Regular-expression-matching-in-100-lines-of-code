/*

 Author: Michid
 Site:   http://michid.wordpress.com/2010/12/06/regular-expression-matching-in-100-lines-of-code/

 */

object Pimps {
  implicit def string2RegExp(s: String) = Str(s)

  implicit def regExpOps(r: RegExp) = new {
    def | (s: RegExp) = Or(r, s)
    def & (s: RegExp) = And(r, s)
    def % = Star(r)
    def %(n: Int) = rep(r, n)
    def ? = Or(Eps, r)
    def ! = Not(r)
    def ++ (s: RegExp) = Cat(r, s)
    def ~ (s: String) = Matcher.matches(r, s)
  }

  implicit def stringOps(s: String) = new {
    def | (r: RegExp) = Or(s, r)
    def | (r: String) = Or(s, r)
    def & (r: RegExp) = And(s, r)
    def & (r: String) = And(s, r)
    def % = Star(s)
    def % (n: Int) = rep(Str(s), n)
    def ? = Or(Eps, s)
    def ! = Not(s)
    def ++ (r: RegExp) = Cat(s, r)
    def ++ (r: String) = Cat(s, r)
    def ~ (t: String) = Matcher.matches(s, t)
  }

  def rep(r: RegExp, n: Int): RegExp =
    if (n <= 0) Star(r)
    else Cat(r, rep(r, n - 1))
}
