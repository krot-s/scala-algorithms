package algorithms

import scala.collection.mutable.ListBuffer
import scala.runtime.RichDouble
import Math._
import java.util.Locale

object MD5 extends App {
  val message = "QWERTY123"
  // 0cc175b9c0f1b6a831c399e269772661

  val buf = new ListBuffer[Byte]
  buf ++= message.getBytes()

  // step 1. add 0x80 to the end
  buf += 0x80.byteValue() // 10000000b

  // pad with zeros 
  do {
    buf += 0
  } while ((buf.length * 8) % 512 != 448)

  // length in bits before padding 
  val origLength = floor((message.getBytes().length * 8) % pow(2, 64)).longValue

  // write original length into last 64 bits (8 bytes), lower 4 bytes go first
  buf += (origLength).toByte
  buf += (origLength >>> 8).toByte
  buf += (origLength >>> 16).toByte
  buf += (origLength >>> 24).toByte
  buf += (origLength >>> 32).toByte
  buf += (origLength >>> 40).toByte
  buf += (origLength >>> 48).toByte
  buf += (origLength >>> 56).toByte  

  // array of contant values
  val k = new Array[Long](64)
  for (i <- 1 to 64) k(i - 1) = (4294967296D * abs(sin(i))).longValue

  // initial vector
  var a0 = 0x67452301
  var b0 = 0xefcdab89
  var c0 = 0x98badcfe
  var d0 = 0x10325476
  
  val s = Array(
	    7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
	    5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
	    4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
	    6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
  )  

  // loop through 512-bit chunks
  for (o <- 0 until buf.length / 64 by 64) {
    // get 16 32-bit integers (4 bytes each)
    var m = Array.fill(16)(0)
    // go throug all bytes and pack each 4 bytes into integer value 
    for (n <- 0 to 63) {
      m(n / 4) = (m(n / 4) >>> 8) | (buf(n) << 24)
    }
    
    var A = a0
    var B = b0
    var C = c0
    var D = d0

    for (i <- 0 until 64) {
      var f: Int = 0
      var g: Int = 0

      i match {
        case x if 0 until 16 contains x =>
          f = (B & C) | (~B & D)
          g = i
        case x if 16 until 32 contains x =>
          f = (D & B) | (~D & C)
          g = (5 * i + 1) % 16
        case x if 32 until 48 contains x =>
          f = B ^ C ^ D
          g = (3 * i + 5) % 16
        case x if 48 until 64 contains x =>
          f = C ^ (B | ~D)
          g = 7 * i % 16
      }

      val temp = B + leftrotate(A + f + k(i).intValue + m(g), s(i)) 
      A = D
      D = C
      C = B
      B = temp
    }

    a0 = a0 + A
    b0 = b0 + B
    c0 = c0 + C
    d0 = d0 + D
  }
  
  val md5 = intToBytes(a0) ++ intToBytes(b0) ++ intToBytes(c0) ++ intToBytes(d0)
  
  println(toHexString(md5))
  
  def intToBytes(value: Int) = {
    val bytes = ListBuffer[Byte](0, 0, 0, 0)
    bytes(0) = (value).byteValue
    bytes(1) = (value >> 8).byteValue
    bytes(2) = (value >> 16).byteValue
    bytes(3) = (value >> 24).byteValue
    bytes
  } 
  
  def toHexString(bytes: ListBuffer[Byte]) = {
    val sb = new StringBuilder
    sb.append("0x")
    for(b <- bytes){
      sb.append(String.format("%02X", b.asInstanceOf[java.lang.Object]))
    }
    sb.toString
  }

  def leftrotate(x: Int, s: Int) = (x << s) | (x >>> (32 - s))
}