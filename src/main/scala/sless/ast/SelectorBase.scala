package sless.ast

case class SelectorBase(thisSelector: String, thisPrettySelector: String)

object SelectorBase{

  def apply(thisSelector: String, thisPrettySelector: String): SelectorBase =
    new SelectorBase(thisSelector, thisPrettySelector)
  def getSelectorString(selector: SelectorBase) : String = selector.thisSelector
  def getSelectorPrettyString(selector: SelectorBase) : String = selector.thisPrettySelector
}
