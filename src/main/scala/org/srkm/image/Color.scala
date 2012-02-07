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

  def searchClosestColor(pallet:List[Color]):Color={
    var min=0;
    var found:Color = null
    var closest:Color = null
    pallet.foreach({p=>
      val d = (Math.pow(red-p.red,2)+Math.pow(green-p.green,2)+Math.pow(blue-p.blue,2)).toInt
      if(d==0){
        found = p
        closest = p
      }else if(min==0 || d<min){
        closest = p
        min = d
      }
    })
    if(found == null){
      closest
    }else{
      found
    }
  }
}

object Color {
  val RED:Int = 0
  val GREEN:Int = 1
  val BLUE:Int = 2
}
