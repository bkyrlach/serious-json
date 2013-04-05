package com.kyrlach.serious.json

object JSONUtils {
  implicit def jsonInt2Int(ji: JSONInt): Int = ji.i
  implicit def jsonStr2Str(js: JSONString): String = js.str
  implicit def jsonDouble2Double(jd: JSONDouble): Double = jd.d
}