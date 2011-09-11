/*

 Author: Michid
 Site:   http://michid.wordpress.com/2010/12/06/regular-expression-matching-in-100-lines-of-code/

 */

object Matcher {
  def matches(r: RegExp, s: String): Boolean = {
    if (s.isEmpty) r.nullable
    else matches(r.derive(s.head), s.tail)
  }
}
