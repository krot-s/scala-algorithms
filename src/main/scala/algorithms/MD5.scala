package algorithms

import scala.collection.mutable.ListBuffer
import scala.runtime.RichDouble
import Math._

object MD5 extends App {
	val message = "abc"		  
	
	val buf = new ListBuffer[Byte] 
	buf ++= message.getBytes()
	
	// step 1. add 0x80 to the end
	buf += 0x80.byteValue() // 10000000b
	
	// pad with zeros 
	do {
	  buf += 0
	} while((buf.length * 8) % 512 != 448)
	  
	// length in bits before padding
	val origLength = floor((message.getBytes().length * 8) % pow(2, 64)).intValue

	// write original length into last 64 bits (8 bytes), lower 4 bytes go first
	buf += (origLength >> 24).toByte
	buf += (origLength >> 16).toByte
	buf += (origLength >> 8).toByte
	buf += origLength.toByte
	buf ++= Array[Byte](0, 0, 0, 0)
	
	// initial vector
	var A = 0x67452301
	var B = 0xefcdab89
	var C = 0x98badcfe
	var D = 0x10325476

	// array of contant values
	val constants = new Array[Long](64)
	for(i <- 1 to 64) constants(i - 1) = (4294967296D * abs(sin(i))).longValue
	
	println(origLength)
	println(buf.length * 8)
	
	def F(x: Long, y: Long, z: Long) = (x & y) | (~x & z)
	def G(x: Long, y: Long, z: Long) = (x & z) | (~z & y) 
	def H(x: Long, y: Long, z: Long) = x ^ y ^ z 
	def I(x: Long, y: Long, z: Long) = y ^ (~z | x) 
}