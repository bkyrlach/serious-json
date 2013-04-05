package com.kyrlach.serious.json

trait JSONReader[A] {
  def toObject(json: String): Option[A]
}