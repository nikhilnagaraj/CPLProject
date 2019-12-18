package sless.dsl

/**
  * https://developer.mozilla.org/en-US/docs/Web/CSS/Reference#Selectors
  */
trait SelectorDSL extends BaseDSL {
  // modifiers
  protected def className(s: Selector, string: String): Selector
  protected def id(s: Selector, string: String): Selector
  protected def attribute(s: Selector, attr: String, value: Value): Selector

  //The given pseudoclass selector and modifications to support PseudoClass selectors with String/Selector arguments
  //pseudo - Pseudoclass selector chosen, arg - argument to said selector
  protected def pseudoClass(s: Selector, string: String): Selector

  protected def pseudoClass(s: Selector, pseudo: String, arg: String): Selector

  protected def pseudoElement(s: Selector, string: String): Selector

  // combinators
  /** -> s + selector { ... } */
  protected def adjacent(s: Selector, selector: Selector): Selector

  /** -> s ~ selector { ... } */
  protected def general(s: Selector, selector: Selector): Selector

  /** -> s > selector { ... } */
  protected def child(s: Selector, selector: Selector): Selector

  /** -> s selector { ... } */
  protected def descendant(s: Selector, selector: Selector): Selector

  // constructors
  protected def group(selectors: Seq[Selector]): Selector
  def tipe(string: String): Selector
  val All: Selector
  def N(selectors: Selector*): Selector = group(selectors.toSeq)

  // bind to declarations
  protected def bindTo(s: Selector, declarations: Seq[Declaration]): Rule

  // shorthand
  implicit class SelectorShorthand(s: Selector) {
    def c(string: String): Selector = className(s, string)
    def ##(string: String): Selector = id(s, string)
    def at(attr: String, value: Value): Selector = attribute(s, attr, value)

    def :|(pseudo: String): Selector = pseudoClass(s, pseudo)

    def :|(pseudo: String, arg: String): Selector = pseudoClass(s, pseudo, arg)
    def ::(pseudoEl: String): Selector = pseudoElement(s, pseudoEl)

    def |~(selector: Selector): Selector = adjacent(s, selector)
    def |+(selector: Selector): Selector = general(s, selector)
    def |>(selector: Selector): Selector = child(s, selector)
    def |-(selector: Selector): Selector = descendant(s, selector)

    //Shorthand to invoke custom Pseudoclass selectors
    def nc(arg: String): Selector = s :| ("nth-child", arg) //nth-child
    def lang(arg: String): Selector = s :| ("lang", arg) //lang
    def nlc(arg: String): Selector = s :| ("nth-last-child", arg) //nth-last-child
    def nt(arg: String): Selector = s :| ("nth-of-type", arg) //nth-of-type
    def nlt(arg: String): Selector = s :| ("nth-last-of-type", arg) //nth-last-of-type

    def apply(declarations: Declaration*): Rule = bindTo(s, declarations.toSeq)
  }
}
