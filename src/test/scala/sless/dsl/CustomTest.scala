package sless.dsl

import org.scalatest.FunSuite

class CustomTest extends FunSuite {

  import CustomTestImplementation.dsl._

  //Tests psuedoClass selectors with string arguments
  test("Pseudoclass args") {
    val backgroundColor = prop("background-color")
    val container = tipe("div") |- tipe("container")
    val p = tipe("p")

    val ex = css(
      N(All.c("class-name1"), All.c("class-name2")) {
        prop("width") := value("100%")
      },
      container {
        backgroundColor := value("blue")
      },
      container.nt("2n+1") {
        backgroundColor := value("red")
      }
    )

    assert(
      CustomTestImplementation.dsl.compile(ex) ===
        """*.class-name1,*.class-name2{width:100%;}div container{background-color:blue;}div container:nth-of-type(2n+1){background-color:red;}""")
  }

  // More extension testing
  test("Nested and partial extension multiple places") {
    val ex = css(
      (All ## "id-name1").extend(All).c("class-name1").extend(tipe("type1")) {
        prop("width") := value("100%")
      },
      All {
        prop("height") := value("95%")
      },
      tipe("p").extend(tipe("type1")) {
        prop("color") := value("green")
      },
      tipe("type1").extend(tipe("p")) {
        prop("height") := value("95%")
      }
    )

    assert(
      CustomTestImplementation.dsl.compile(ex) ===
        """*#id-name1.class-name1{width:100%;}*,*#id-name1{height:95%;}p,type1{color:green;}type1,*#id-name1.class-name1,p{height:95%;}""")
  }

}
