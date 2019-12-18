package sless.ast

import sless.dsl.SelectorDSL

trait SelectorBaseTrait extends BaseImpl with SelectorDSL{
  override protected def className(s: SelectorBase, string: String): SelectorBase ={
    val selectorString = s.thisSelector + "." + string
    val selectorPrettyString = s.thisPrettySelector + "." + string
    SelectorBase(selectorString, selectorPrettyString, s.extendSelector)
  }

  override protected def id(s: SelectorBase, string: String): SelectorBase = {
    val selectorString = s.thisSelector + "#" + string
    val selectorPrettyString = s.thisPrettySelector + "#" + string
    SelectorBase(selectorString, selectorPrettyString, s.extendSelector)
  }

  override protected def attribute(s: SelectorBase, attr: String, value: ValueBase): SelectorBase = {
    val selectorString = s.thisSelector + "[" + attr + "=" + value.thisValue + "]"
    val selectorPrettyString = s.thisPrettySelector + "[" + attr + "=" + value.thisValue + "]"
    SelectorBase(selectorString, selectorPrettyString, s.extendSelector)
  }

  override protected def pseudoClass(s: SelectorBase, pseudo: String): SelectorBase = {
    val selectorString = s.thisSelector + ":" + pseudo
    val selectorPrettyString = s.thisPrettySelector + ":" + pseudo
    SelectorBase(selectorString, selectorPrettyString, s.extendSelector)
  }

  override protected def pseudoClass(s: SelectorBase, pseudo: String, arg: String): SelectorBase = {
    val selectorString = s.thisSelector + ":" + pseudo + "(" + arg + ")"
    val selectorPrettyString = s.thisPrettySelector + ":" + pseudo + "(" + arg + ")"
    SelectorBase(selectorString, selectorPrettyString, s.extendSelector)
  }

  override protected def pseudoElement(s: SelectorBase, string: String): SelectorBase = {
    val selectorString = s.thisSelector + "::" + string
    val selectorPrettyString = s.thisPrettySelector + "::" + string
    SelectorBase(selectorString, selectorPrettyString, s.extendSelector)
  }

  /** -> s + selector { ... } */
  override protected def adjacent(s: SelectorBase, selector: SelectorBase): SelectorBase = {
    val selectorString = s.thisSelector + "+" + selector.thisSelector
    val selectorPrettyString = s.thisPrettySelector + " + " + selector.thisPrettySelector
    SelectorBase(selectorString, selectorPrettyString, s.extendSelector ++ selector.extendSelector)
  }

  /** -> s ~ selector { ... } */
  override protected def general(s: SelectorBase, selector: SelectorBase): SelectorBase = {
    val selectorString = s.thisSelector + "~" + selector.thisSelector
    val selectorPrettyString = s.thisPrettySelector + " ~ " + selector.thisPrettySelector
    SelectorBase(selectorString, selectorPrettyString, s.extendSelector ++ selector.extendSelector)
  }

  /** -> s > selector { ... } */
  override protected def child(s: SelectorBase, selector: SelectorBase): SelectorBase = {
    val selectorString = s.thisSelector + ">" + selector.thisSelector
    val selectorPrettyString = s.thisPrettySelector + " > " + selector.thisPrettySelector
    SelectorBase(selectorString, selectorPrettyString, s.extendSelector ++ selector.extendSelector)
  }

  /** -> s selector { ... } */
  override protected def descendant(s: SelectorBase, selector: SelectorBase): SelectorBase = {
    val selectorString = s.thisSelector + " " + selector.thisSelector
    val selectorPrettyString = s.thisPrettySelector + " " + selector.thisPrettySelector
    SelectorBase(selectorString, selectorPrettyString, s.extendSelector ++ selector.extendSelector)
  }

  override protected def group(selectors: Seq[SelectorBase]): SelectorBase = {
    val selectorString = Utility.map[SelectorBase, String](_.thisSelector, selectors).mkString(",")
    val selectorPrettyString = Utility.map[SelectorBase, String](_.thisPrettySelector, selectors).mkString(", ")
    var extensions = Seq[(SelectorBase, SelectorBase)]()
    for (selector <- selectors) {
      extensions = extensions ++ selector.extendSelector
    }
    SelectorBase(selectorString, selectorPrettyString, extensions)
  }


  override def tipe(string: String): SelectorBase =
    SelectorBase(string, string, Seq[(SelectorBase, SelectorBase)]())

  override val All: SelectorBase =
    SelectorBase("*", "*", Seq[(SelectorBase, SelectorBase)]())

  override protected def bindTo(s: SelectorBase, declarations: Seq[DeclarationBase]): RuleBase
  = RuleBase(s, declarations)
}