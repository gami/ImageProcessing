package org.srkm.image
import scala.collection.mutable.ListBuffer

class ColorCube(colors:List[Color]) {
  val cs=colors
  var minR = 255
  var maxR = 0
  var minG = 255
  var maxG = 0
  var minB = 255
  var maxB = 0

  colors.foreach({color=>
    if(color.red < minR) minR = color.red
    if(color.red > maxR) maxR = color.red
    if(color.green < minG) minG = color.green
    if(color.green > maxG) maxG = color.green
    if(color.blue < minB) minB = color.blue
    if(color.blue > maxB) maxB = color.blue
  })

  def divide():List[ColorCube]={
    val cut = largestEdge()
    val med = median(cut)
    return divideBy(cut, med)
  }

  def divideBy(cut:Int, median:Int):List[ColorCube]={
    var list0:ListBuffer[Color] = new ListBuffer()
    var list1:ListBuffer[Color] = new ListBuffer()

    colors.foreach({c=>
      if(c.get(cut) < median){
        list0 += c
      }else{
        list1 += c
      }
    })

    if(list0.size > 0 && list1.size > 0){
      List(new ColorCube(list0.toList), new ColorCube(list1.toList));
    }else{
      Nil
    }
  }

  def median(cut:Int)={
    val edge = colors.map(_.get(cut)).sort((a,b)=>a > b)
    edge.take(edge.size/2+1).last
  }

  def largestEdge():Int={
    val diffR = maxR-minR
    val diffG = (maxG-minG) * 0.8
    val diffB = (maxB-minB) * 0.5

    if(diffG >= diffB){
      if(diffR >= diffG){
        Color.RED
      }else{
        Color.GREEN
      }
    }else{
     if(diffR >=diffB){
       Color.RED
     } else{
       Color.BLUE
     }
    }
  }

  def colorCount:Int = cs.size

  def average():Color={
    var sumR = 0
    var sumG = 0
    var sumB = 0
    colors.foreach({c=>
      sumR += c.get(Color.RED)
      sumG += c.get(Color.GREEN)
      sumB += c.get(Color.BLUE)
    })
    val c = new Color(sumR/colors.size, sumG/colors.size, sumB/colors.size)
    c.searchClosestColor(colors)
  }
}