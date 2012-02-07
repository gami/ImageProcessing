package org.srkm.image
import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

object Quantizer {

  //usage Quantizer <image-src> <image-dest> <max-color>
  def main(args:Array[String]):Unit = {
    val srcPath = args(0)
    val destPath = args(1)
    val maxcolor = args(2).toInt;

    val src = ImageIO.read(new File(srcPath))

    val colors = extractColors(src)
    println("colors:" + colors.size)
    val pallet = medianCut(colors, maxcolor);
    println("pallet:" + pallet.size)
    val dest = quantize(src, pallet)

    ImageIO.write(dest, "png", new File(destPath))
  }

  def extractColors(src:BufferedImage):List[Color]={
    val w = src.getWidth()
    val h = src.getHeight()

    var colors = new HashMap[Int, Color]
    (0 to w-1).foreach({x=>
      (0 to h-1).foreach({y=>
        val rgb = src.getRGB(x,y)
        val r = (rgb >> 16) & 0xFF
        val g = (rgb >> 8)  & 0xFF
        val b = rgb         & 0xFF
        val c = new Color(r,g,b)
        if(!colors.contains(rgb)){
          colors.put(rgb, c)
        }
      })
    });
    return colors.values.toList;
  }

  def medianCut(colors:List[Color], maxColor:Int):List[Color]={
    val cube = new ColorCube(colors)
    val divided = divideUntil(List(cube), maxColor)
    return divided.toList.map(_.average);
  }

  def divideUntil(cubes:List[ColorCube], limit:Int):List[ColorCube]={
    cubes match {
      case c if c.size >= limit => cubes
      case _ => {
        val largest = largestCube(cubes)
        val divided = largest.divide()
        if(divided.size < 2){
          cubes
        }else{
          val result = cubes.remove(_==largest) ::: divided
          divideUntil(result,limit)
        }
      }
    }
  }

  def largestCube(cubes:List[ColorCube]):ColorCube={
    var max:ColorCube = null
    var maxCount = 0
    cubes.foreach({x=>
      if(x.colorCount > maxCount){
        max = x
        maxCount = x.colorCount
      }
    })
    max
  }

  def quantize(src:BufferedImage, pallet:List[Color]):BufferedImage={

    val w = src.getWidth()
    val h = src.getHeight()

    val dest = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_INDEXED)
    (0 to w-1).foreach({x=>
      (0 to h-1).foreach({y=>
        val rgb = src.getRGB(x,y)
        val r = (rgb >> 16) & 0xFF
        val g = (rgb >> 8)  & 0xFF
        val b = rgb         & 0xFF

        val c = new Color(r,g,b)
        dest.setRGB(x, y, c.searchClosestColor(pallet).toInt)
      })
    })
    return dest;
  }
}