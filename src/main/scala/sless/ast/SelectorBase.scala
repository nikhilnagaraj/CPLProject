package sless.ast

case class SelectorBase(thisSelector: String, thisPrettySelector: String) {
  var extendSelector: Seq[(SelectorBase, SelectorBase)] = Seq[(SelectorBase, SelectorBase)]()
}

object SelectorBase{

  def apply(thisSelector: String, thisPrettySelector: String, extensions: Seq[(SelectorBase, SelectorBase)]): SelectorBase = {
    var s = new SelectorBase(thisSelector, thisPrettySelector)
    s.extendSelector = extensions
    s
  }

  def constructExtension(s1: SelectorBase, s2: SelectorBase): SelectorBase = {
    val selectorString = s2.thisSelector + "," + s1.thisSelector
    val selectorPrettyString = s2.thisPrettySelector + ", " + s1.thisPrettySelector
    SelectorBase(selectorString, selectorPrettyString)
  }

  def removeDuplicates(selectorList: Seq[SelectorBase]): Seq[SelectorBase] = {
    /*
    Input: List of Selectors (Each selector might be a list itself)
    Output: A list of unique selectors matching the merge extension specifications.
     */

    var selectorOrderedList = Seq[SelectorBase]()
    var i = selectorList.size - 1

    for (selectorI <- selectorList.reverse) {
      val subSelectors = selectorI.thisSelector.split(",")
      val subPrettySelectors = selectorI.thisPrettySelector.split(", ")
      if (subSelectors.size > 1) {
        var remainingSelector = Seq[String]()
        var remainingPrettySelector = Seq[String]()
        for ((subSelector, subPrettySelector) <- subSelectors zip subPrettySelectors) {
          if (Utility.deleteAtIndex(selectorList, i).exists(item =>
            item.thisSelector.split(",").contains(subSelector))) {
            if (!selectorOrderedList.exists(item => item.thisSelector.split(",").contains(subSelector)))
              selectorOrderedList = selectorOrderedList :+ SelectorBase(subSelector, subPrettySelector)
          } else {
            remainingSelector = remainingSelector :+ subSelector
            remainingPrettySelector = remainingPrettySelector :+ subPrettySelector
          }
        }
        selectorOrderedList = selectorOrderedList :+ SelectorBase(remainingSelector.mkString(","),
          remainingPrettySelector.mkString(", "))
      } else {
        if (!selectorOrderedList.exists(item => item.thisSelector.split(",").contains(subSelectors(0))))
          selectorOrderedList = selectorOrderedList :+ SelectorBase(subSelectors(0), subPrettySelectors(0))
      }
      i = i - 1
    }
    selectorOrderedList = selectorOrderedList.reverse
    selectorOrderedList
  }
}
