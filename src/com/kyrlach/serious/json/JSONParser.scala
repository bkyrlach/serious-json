package com.kyrlach.serious.json

import scala.util.parsing.combinator.RegexParsers

sealed trait JSONExpression 
case class JSONNull() extends JSONExpression
case class JSONString(str: String) extends JSONExpression
case class JSONInt(i: Int) extends JSONExpression
case class JSONDouble(d: Double) extends JSONExpression
case class JSONArray(l: List[JSONExpression]) extends JSONExpression {
  def get[A](): Option[List[A]] = try {
    Some(l.map(_.asInstanceOf[A]))
  } catch {
    case _: Throwable => None
  }
}
case class JSONObject(data: Map[Symbol, JSONExpression]) extends JSONExpression {
  def get[A](key: Symbol): Option[A] = try {
    Some(data(key).asInstanceOf[A])
  } catch {
    case _: Throwable => None
  }
}

object JSONParser extends RegexParsers {
  def nul: Parser[JSONNull] = "null" ^^ { x => JSONNull() }
  def key: Parser[Symbol] = "\"" ~> """([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ "\"" ^^ { str => Symbol(str) }
  def str: Parser[JSONString] = "\"" ~> """([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ "\"" ^^ { str => JSONString(str) }
  def inum: Parser[JSONInt] = """-?\d+""".r ^^ { str => JSONInt(str.toInt) }  
  def fnum: Parser[JSONDouble] = """-?\d*\.\d+""".r ^^ { str => JSONDouble(str.toDouble) }
  def array: Parser[JSONArray] = "[" ~> repsep(value, ",") <~ "]" ^^ { x => JSONArray(x) } 
  def value: Parser[JSONExpression] = nul | str | inum | fnum | obj | array
  def keyval: Parser[(Symbol, JSONExpression)] = key ~ ":" ~ value ^^ ( x => x._1._1 -> x._2 )
  
  def obj: Parser[JSONObject] = "{" ~> repsep(keyval, ",") <~ "}" ^^ { x => JSONObject(x.foldLeft[Map[Symbol, JSONExpression]](Map.empty){(acc, x) => acc + x }) }
}