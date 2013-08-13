package com.rayrobdod.testing.javaScriptObjectNotation.parser.listeners

import com.rayrobdod.javaScriptObjectNotation.parser._
import com.rayrobdod.javaScriptObjectNotation.parser.listeners._
import org.scalatest.{FunSpec}
import java.io.StringWriter;

/**
 * @author Raymond Dodge
 * @version 2013 Jun 25
 */
class MinifierTest extends FunSpec
{
	def minify(s:String):String = {
		val w = new StringWriter
		val l = new Minifier(w)
		
		JSONParser.parse(l, s)
		w.toString
	}
	
	describe ("Minifier") {}
	
	describe ("JSONParser.parse(Minifier)") {
		it ("should remove whitespace from empty array") {
			expectResult("[]")(minify("  [  ]  "))
		}
		
		it ("should trim whitespace from empty object") {
			expectResult("{:}")(minify("  {  :  }  "))
		}
		
		it ("should remove whitespace from around array values") {
			expectResult("{1,2,3}")(minify("{ 1, 2, 3 }"))
		}
		
		it ("should remove whitespace from around object values") {
			expectResult("""{"1":true,"2":false}""")(
				minify("""{ "1" : true , "2" : false }"""))
		}
		
		it ("should recurse") {
			expectResult("[[1,2],[3,4]]")(
				minify(" [ [ 1 , 2 ] , [ 3 , 4 ] ] "))
		}
		
		// stretch goal
		// when is 'if possible'? all UTF-8? ASCII?
		ignore ("should unescape json string values") {
			expectResult("{\"A\"}")(minify("""{ "\""" + """u0041" }"""))
		}
	}
}
