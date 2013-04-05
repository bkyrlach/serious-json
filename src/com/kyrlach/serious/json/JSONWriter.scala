package com.kyrlach.serious.json

trait JSONWriter[A] {
  def toJSON(a: A): String
}