package tiny

import org.scalatest.FlatSpec

class ScannerTest extends FlatSpec {

  "splitIdentifier" should "work correctly2" in {
    assertResult("asdf32") {
      Scanner.splitIdentifier("asdf32*(&")._1
    }
    assertResult("as_DF") {
      Scanner.splitIdentifier("as_DF)")._1
    }
    assertResult("p") {
      Scanner.splitIdentifier("p*x")._1
    }
    assertResult("kjl") {
      Scanner.splitIdentifier("kjl++{")._1
    }
  }

  "splitNumber" should "work correctly2" in {
    assertResult("1234") {
      Scanner.splitNumber("1234asdf")._1
    }
    assertResult("64.34") {
      Scanner.splitNumber("64.34jpdf")._1
    }
    assertResult("00.34") {
      Scanner.splitNumber("00.34t")._1
    }
  }

  val source: String =
    """
      |read u;
      |read v; { input two integers }
      |if v=0 then v:=0 { do nothing }
      |else
      |  repeat
      |    temp := v;
      |    v:=u-u/v*v;
      |    u:=temp
      |  until v=0
      |end;
      |write u{output gcd of original u & v}
    """.stripMargin

  "split" should "work correctly2" in {

    val result = List(
      "read", "u", ";", "read", "v", ";", "if", "v", "=", "0", "then",
      "v", ":=", "0", "else", "repeat", "temp", ":=", "v", ";", "v", ":=",
      "u", "-", "u", "/", "v", "*", "v", ";", "u", ":=", "temp", "until",
      "v", "=", "0", "end", ";", "write", "u",
    )
    assertResult(result) {
      Scanner.split(source).get
    }
  }
}
