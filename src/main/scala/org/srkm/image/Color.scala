package org.srkm.image

class Color(r:Int, g:Int, b:Int) {
  def red=r;
  def green=g;
  def blue=b;
  
  def get(w:Int)={
    w match {
      case Color.RED => red;
      case Color.GREEN => green;
      case Color.BLUE => blue;
    }
  }
  
  def toInt():Int={
    (r<<16)+(g<<8)+b
  }
}

object Color {
  val RED:Int = 0
  val GREEN:Int = 1
  val BLUE:Int = 2
}
