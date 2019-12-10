package sless.dsl

import sless.ast.BaseObject

object LessVariableImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with Compilable
  val dsl: DSL = BaseObject

  import dsl._

  /**
    * Create a Css Declaration for a background-color in the given color
    * -> background-color: $color;
    * @param color
    * @return
    */
  def coloredBg(color: String): dsl.Declaration =
    dsl.prop("background-color").:=(dsl.value(color))

  /**
    * Create a css sheet that colors an element with the given id in red
    * -> *#id { background-color: red; }
    * ! Use coloredBg in your implementation
    * @param id
    * @return
    */
  def colorNamedRed(id: String): dsl.Css =
    dsl.css((dsl.All.##(id)).apply(coloredBg("red")))

  /**
    * Create a rule for the given element type that has an aspect ratio of 2/1
    * -> elementType { height: $height; width: $height * 2; }
    * @param height
    * @return
    */
  def doubledWidth(elementType: String, height: Int): dsl.Rule =
    dsl.tipe(elementType).apply(dsl.prop("height").:=(dsl.value(height.toString)),
                                dsl.prop("width").:=(dsl.value((height*2).toString)))

}
