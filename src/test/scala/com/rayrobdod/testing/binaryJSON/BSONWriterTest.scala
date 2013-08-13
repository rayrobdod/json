package com.rayrobdod.testing.binaryJSON

import com.rayrobdod.binaryJSON.parser._
import com.rayrobdod.binaryJSON.parser.listeners._
import com.rayrobdod.binaryJSON.parser.decoders.ToJavaCollectionBSONDecoder
import com.rayrobdod.testing.binaryJSON.parser.listeners.commonObjects
import com.rayrobdod.binaryJSON.{BSONWriter, NulCharacterInKeyException}
import org.scalatest.{FunSuite, FunSpec}
import org.scalatest.prop.PropertyChecks
import java.text.ParseException;
import scala.collection.JavaConversions.{mapAsJavaMap, seqAsJavaList}
import java.io.{DataInputStream, ByteArrayInputStream}

/**
 * @author Raymond Dodge
 * @version 2013 Aug 03
 */
class BSONWriterTest extends FunSpec
{
	describe ("BSONWriter") {
		it ("should create a correct empty list") {
			expectResult(
				Seq(5,0,0,0,0).map{_.byteValue}.toSeq
			)(
				BSONWriter.encode(Seq.empty).toSeq
			)
		}
		it ("should create a correct empty map") {
			expectResult(
				Seq(5,0,0,0,0).map{_.byteValue}.toSeq
			)(
				BSONWriter.encode(mapAsJavaMap(Map.empty[String,Object])).toSeq
			)
		}
		it ("should create a correct one-element list") {
			expectResult(
				Seq(12,0,0,0,0x10,'0',0,5,0,0,0,0).toSeq
			)(
				BSONWriter.encode(Seq(5)).toSeq
			)
		}
		it ("should create a correct one-pair map") {
			expectResult(
				Seq(12,0,0,0,0x10,'a',0,2,0,0,0,0).toSeq
			)(
				BSONWriter.encode(mapAsJavaMap(Map("a" → 2))).toSeq
			)
		}
		it ("should create a correct multi-element list") {
			expectResult(
				Seq(40,0,0,0,
					0x10,'0',0, 0,0,0,0,
					0x10,'1',0, 1,0,0,0,
					0x10,'2',0, 2,0,0,0,
					0x10,'3',0, 3,0,0,0,
					0x10,'4',0, 4,0,0,0,
				0).toSeq
			)(
				BSONWriter.encode(0 to 4).toSeq
			)
		}
		it ("should create a correct multi-pair map") {
			val orig = (1 to 5).map{(a) => ((a.toString, a))}.toMap
			val string = BSONWriter.encode(mapAsJavaMap(orig));
			
			val l = new ToHashMap(new ToJavaCollectionBSONDecoder)
			BSONParser.parse(l, commonObjects.DataInput(string))
			
			expectResult(mapAsJavaMap(orig))(l.getResult)
		}
		it ("should recurse") {
			expectResult(
				Seq(13,0,0,0,
					0x04,'0',0, 5,0,0,0,0,
				0).toSeq
			)(
				BSONWriter.encode(Seq(new java.util.ArrayList())).toSeq
			)
		}
		it ("should do keywords") {
			expectResult(
				Seq(16,0,0,0,
					0x08,'0',0,1,
					0x08,'1',0,0,
					0x0A,'2',0,
				0).toSeq
			)(
				BSONWriter.encode(Seq(true,false,null)).toSeq
			)
		}
		it ("should do strings") {
			expectResult(
				Seq(18,0,0,0,
					0x02,'0',0,6,0,0,0,'h','e','l','l','o',0,
				0).toSeq
			)(
				BSONWriter.encode(Seq("hello")).toSeq
			)
		}
		it ("should error when key contains NUL") {
			intercept[NulCharacterInKeyException] {
				BSONWriter.encode(Map("\0" -> "NUL"))
			}
			
			// log(BSONWriter.encode(Map("\0" -> "NUL")).toSeq)
		}
	}
}
