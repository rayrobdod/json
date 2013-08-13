package com.rayrobdod.testing.javaScriptObjectNotation

import com.rayrobdod.javaScriptObjectNotation.parser._
import com.rayrobdod.javaScriptObjectNotation.parser.listeners._
import com.rayrobdod.javaScriptObjectNotation.parser.decoders.ToJavaCollectionJSONDecoder
import com.rayrobdod.javaScriptObjectNotation.JSONWriter
import org.scalatest.{FunSuite, FunSpec}
import org.scalatest.prop.PropertyChecks
import java.text.ParseException;
import scala.collection.JavaConversions.{mapAsJavaMap, seqAsJavaList}

/**
 * @author Raymond Dodge
 * @version 2013 Aug 03
 */
class JSONWriterTest extends FunSpec
{
	describe ("JSONWriter") {
		it ("should create a correct empty list") {
			expectResult("[]")(JSONWriter.encode(Seq.empty))
		}
		it ("should create a correct empty map") {
			expectResult("{}")(JSONWriter.encode(mapAsJavaMap(Map.empty[String,Object])))
		}
		it ("should create a correct one-element list") {
			expectResult("[ 5]")(JSONWriter.encode(Seq(5)))
		}
		it ("should create a correct one-pair map") {
			expectResult("{ \"1\":2}")(JSONWriter.encode(mapAsJavaMap(Map( "1" → 2))))
		}
		it ("should create a correct multi-element list") {
			expectResult("[ 1,2,3,4,5]")(JSONWriter.encode(Seq(1,2,3,4,5)))
		}
		it ("should create a correct multi-pair map") {
			val orig = (1 to 5).map{(a) => ((a.toString, a))}.toMap
			val string = JSONWriter.encode(mapAsJavaMap(orig));
			
			val l = new ToScalaCollection(new ToJavaCollectionJSONDecoder)
			JSONParser.parse(l, string)
			
			expectResult(orig)(l.resultMap)
		}
		it ("should recurse") {
			val orig = Seq(Seq(1, 2)).map{seqAsJavaList}
			val string = JSONWriter.encode(orig);
			
			expectResult("[ [ 1,2]]")(string)
		}
		it ("should do keywords") {
			val orig = Seq(true, false, null)
			val string = JSONWriter.encode(orig);
			
			expectResult("[ true,false,null]")(string)
		}
		it ("should do strings") {
			val orig = Seq("Hello")
			val string = JSONWriter.encode(orig);
			
			expectResult("[ \"Hello\"]")(string)
		}
	}
}
