package sless.ast

import sless.dsl.SelectorDSL

trait SelectorBaseTrait extends BaseImpl with SelectorDSL{
  override protected def className(s: SelectorBase, string: String): SelectorBase ={
    val selectorString = SelectorBase.getSelectorString(s) + "." + string
     SelectorBase(selectorString, selectorString)
  }

  override protected def id(s: SelectorBase, string: String): SelectorBase = {
    val selectorString = SelectorBase.getSelectorString(s) + "#" + string
     SelectorBase(selectorString, selectorString)
  }

  override protected def attribute(s: SelectorBase, attr: String, value: ValueBase): SelectorBase = {
    val selectorString = SelectorBase.getSelectorString(s) +  "[" + attr + "=" + ValueBase.getValueString(value) + "]"
     SelectorBase(selectorString, selectorString)
  }

  override protected def pseudoClass(s: SelectorBase, string: String): SelectorBase = {
    val selectorString = SelectorBase.getSelectorString(s) + ":" + string
     SelectorBase(selectorString, selectorString)
  }

  override protected def pseudoElement(s: SelectorBase, string: String): SelectorBase = {
    val selectorString = SelectorBase.getSelectorString(s) + "::" + string
     SelectorBase(selectorString, selectorString)
  }

  /** -> s + selector { ... } */
  override protected def adjacent(s: SelectorBase, selector: SelectorBase): SelectorBase = {
    val selectorString = SelectorBase.getSelectorString(s) + "+" + SelectorBase.getSelectorString(selector)
    val selectorPrettyString = SelectorBase.getSelectorString(s) + " + " + SelectorBase.getSelectorString(selector)
     SelectorBase(selectorString, selectorPrettyString)
  }

  /** -> s ~ selector { ... } */
  override protected def general(s: SelectorBase, selector: SelectorBase): SelectorBase = {
    val selectorString = SelectorBase.getSelectorString(s) + "~" + SelectorBase.getSelectorString(selector)
    val selectorPrettyString = SelectorBase.getSelectorString(s) + " ~ " + SelectorBase.getSelectorString(selector)
     SelectorBase(selectorString, selectorPrettyString)
  }

  /** -> s > selector { ... } */
  override protected def child(s: SelectorBase, selector: SelectorBase): SelectorBase = {
    val selectorString = SelectorBase.getSelectorString(s) + ">" + SelectorBase.getSelectorString(selector)
    val selectorPrettyString = SelectorBase.getSelectorString(s) + " > " + SelectorBase.getSelectorString(selector)
     SelectorBase(selectorString, selectorPrettyString)
  }

  /** -> s selector { ... } */
  override protected def descendant(s: SelectorBase, selector: SelectorBase): SelectorBase = {
    val selectorString = SelectorBase.getSelectorString(s) + " " + SelectorBase.getSelectorString(selector)
     SelectorBase(selectorString, selectorString)
  }

  override protected def group(selectors: Seq[SelectorBase]): SelectorBase = {
    val selectorString = Utility.map[SelectorBase, String](SelectorBase.getSelectorString,selectors).mkString(",")
    val selectorPrettyString = Utility.map[SelectorBase, String](SelectorBase.getSelectorString,selectors).mkString(", ")
     SelectorBase(selectorString, selectorPrettyString)
  }


  override def tipe(string: String): SelectorBase =
     SelectorBase(string, string)

  override val All: SelectorBase =
     SelectorBase("*", "*")

  override protected def bindTo(s: SelectorBase, declarations: Seq[DeclarationBase]): RuleBase = RuleBase(s,declarations)
}