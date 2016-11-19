/*
	Copyright (c) 2015-2016, Raymond Dodge
	All rights reserved.
	
	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:
		* Redistributions of source code must retain the above copyright
		  notice, this list of conditions and the following disclaimer.
		* Redistributions in binary form must reproduce the above copyright
		  notice, this list of conditions and the following disclaimer in the
		  documentation and/or other materials provided with the distribution.
		* Neither the name "<PRODUCT NAME>" nor the names of its contributors
		  may be used to endorse or promote products derived from this software
		  without specific prior written permission.
	
	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
	ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
	DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
	DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
	(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
	LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
	ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
	SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package com.rayrobdod.json.builder;

import scala.collection.immutable.Seq
import scala.util.{Either, Left, Right}
import org.scalatest.FunSpec;
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.{StringOrInt, ParserRetVal}
import com.rayrobdod.json.union.ParserRetVal.Complex
import com.rayrobdod.json.parser.{Parser, IdentityParser, PrimitiveSeqParser}
import com.rayrobdod.json.builder.PiecewiseBuilder.KeyDef

class PiecewiseBuilderTest extends FunSpec {
	import PiecewiseBuilderTest.Person;
	
	describe("PiecewiseBuilder") {
		it ("inits correctly") {
			assertResult(new Person("me", 4)){
					new PiecewiseBuilder(new Person("me", 4)).init
			}
		}
		it ("Acts upon provided keydef") {
			val name = "Anony Mouse"
			assertResult(Right(new Person(name, 0))){
				new PiecewiseBuilder(new Person("", 0))
						.addDef("name", new KeyDef[String, String, Person]{def apply[I](s:Person, i:I, p:Parser[String, String, I]) = {Right(s.copy(name = p.parsePrimitive(i).fold({x => ""}, {x => x})))}})
						.apply(new Person("", 0), "name", name, new IdentityParser[String])
			}
		}
		it ("Acts upon provided keydef (2)") {
			val age = 9001
			assertResult(Right(new Person("", age))){
				new PiecewiseBuilder(new Person("", 0))
						.addDef("age", new KeyDef[String, Int, Person]{def apply[I](s:Person, i:I, p:Parser[String, Int, I]) = {Right(s.copy(age = p.parsePrimitive(i).fold({x => 0}, {x => x})))}})
						.apply(new Person("", 0), "age", age, new IdentityParser[Int])
			}
		}
		it ("Acts upon provided keydef (partitionedPrimitive)") {
			val age = 9001
			assertResult(Right(new Person("", age))){
				new PiecewiseBuilder(new Person("", 0))
						.addDef("age", PiecewiseBuilder.partitionedPrimitiveKeyDef[String, Int, Person, Int]({case x:Int => Right(x)}, {(f,i) => f.copy(age = i)}))
						.apply(new Person("", 0), "age", age, new IdentityParser[Int])
			}
		}
		it ("Throws excpetion on unknown key") {
			assertResult(Left("PiecewiseBuilder has no KeyDef for given key", 0)){
				new PiecewiseBuilder[String, String, Person](new Person("", 0))
						.apply(new Person("", 0), "asdfjkl;", "hello", new IdentityParser[String])
			}
		}
		it ("ignores unknown key after call to ignoreUnknownKeys") {
			assertResult(Right(new Person("", 0))){
				new PiecewiseBuilder[String, String, Person](new Person("", 0)).ignoreUnknownKeys
						.apply(new Person("", 0), "asdfjkl;", "hello", new IdentityParser[String])
			}
		}
	}
	
	describe("PiecewiseBuilder + JsonParser") {
		import com.rayrobdod.json.parser.JsonParser
		import com.rayrobdod.json.union.JsonValue._
		
		it ("works") {
			val builder = new PiecewiseBuilder[StringOrInt, JsonValue, Person](new Person("", 0))
				.addDef("name", new KeyDef[StringOrInt, JsonValue, Person]{ def apply[I](s:Person, i:I, p:Parser[StringOrInt, JsonValue, I]) = {p.parsePrimitive(i).right.flatMap{_ match {case JsonValueString(i) => Right(s.copy(name = i)); case ex => Left("name not string: " + ex, 0)}}}})
				.addDef("age", new KeyDef[StringOrInt, JsonValue, Person]{ def apply[I](s:Person, i:I, p:Parser[StringOrInt, JsonValue, I]) = {p.parsePrimitive(i).right.flatMap{_ match {case JsonValueNumber(i,t) => Right(s.copy(age = t.tryToInt(i).get)); case ex => Left("age not number: " + ex, 0)}}}})
			
			assertResult(Person("nqpppnl",1)){
				new JsonParser().parse(builder, 
					"""{"name":"nqpppnl","age":1}"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("nested") {
			val exp = Seq(Person("a", 5), Person("b", 6))
			
			val personBuilder = new PiecewiseBuilder[StringOrInt, JsonValue, Person](new Person("", 0))
				.addDef("name", new KeyDef[StringOrInt, JsonValue, Person]{ def apply[I](s:Person, i:I, p:Parser[StringOrInt, JsonValue, I]) = {p.parsePrimitive(i).right.flatMap{_ match {case JsonValueString(i) => Right(s.copy(name = i)); case ex => Left("name not string: " + ex, 0)}}}})
				.addDef("age", new KeyDef[StringOrInt, JsonValue, Person]{ def apply[I](s:Person, i:I, p:Parser[StringOrInt, JsonValue, I]) = {p.parsePrimitive(i).right.flatMap{_ match {case JsonValueNumber(x, t) => t.tryToInt(x).map{i => (s.copy(age = i))}.toRight(("age not integer", 0)); case ex => Left("age not number: " + ex, 0)}}}})
			
			val seqBuilder = new PiecewiseBuilder[StringOrInt, JsonValue, Seq[Person]](
				Nil,
				new KeyDef[StringOrInt, JsonValue, Seq[Person]]{ def apply[I](s:Seq[Person], i:I, p:Parser[StringOrInt, JsonValue, I]) = {p.parse(personBuilder, i).fold({x => Right(x)},{x => Left("ASFD",0)},{(a,b) => Left(a,b)}).right.map{y => s :+ y}}}
			)
				
			assertResult(exp){
				new JsonParser().parse(seqBuilder, 
					"""[{"name":"a","age":5},{"name":"b","age":6}]"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("does the example provided in the scaladocs") {
			import JsonValue.JsonValueString
			import PiecewiseBuilder._
			
			case class Foo(a:String, b:Seq[JsonValue], c:String)
			val fooBuilder = (new PiecewiseBuilder[StringOrInt, JsonValue, Foo](new Foo("", Seq.empty, ""))
				.addDef(StringOrInt("a"), partitionedPrimitiveKeyDef({case JsonValueString(x) => Right(x)}, {(f:Foo, x:String) => f.copy(a = x)}))
				.addDef(StringOrInt("c"), partitionedPrimitiveKeyDef({case JsonValueString(x) => Right(x)}, {(f:Foo, x:String) => f.copy(c = x)}))
				.addDef(StringOrInt("b"), partitionedComplexKeyDef(new PrimitiveSeqBuilder[JsonValue], {(f:Foo, x:Seq[JsonValue]) => Right(f.copy(b = x))}))
			)
			val jsonParser = new JsonParser
			
			assertResult(Complex(Foo("", Seq.empty, ""))){
				jsonParser.parse(fooBuilder, """{"a":"","b":[]}""")
			}
			assertResult(Complex(Foo("qwer", Seq(JsonValueString("z"), JsonValueString("x"), JsonValueString("c")), "asdf"))){
				jsonParser.parse(fooBuilder, """{"a":"qwer","b":["z","x","c"],"c":"asdf"}""")
			}
		}
	}
	
	describe("KeyDef") {
		import PiecewiseBuilder._
		
		it ("ignoreKeyDef") {
			assertResult(Right("abc")){
				ignoreKeyDef.apply("abc", null, null)
			}
		}
		it ("throwKeyDef") {
			assertResult(Left("PiecewiseBuilder has no KeyDef for given key", 0)){
				throwKeyDef.apply("abc", null, null)
			}
		}
		it ("partitionedPrimitiveKeyDef (isDefinedAt)") {
			val builder = partitionedPrimitiveKeyDef[Any, String, Option[Int], Int]({case "abc" => Right(3)}, {(a,b) => Some(b)})
			assertResult(Right(Some(3))){
				builder.apply(None, "abc", new IdentityParser[String])
			}
		}
		it ("partitionedPrimitiveKeyDef (not isDefinedAt)") {
			val builder = partitionedPrimitiveKeyDef[Any, String, Option[Int], Int]({case "abc" => Right(3)}, {(a,b) => Some(b)})
			assertResult(Left("Unexpected value: asdf", 0)){
				builder.apply(None, "asdf", new IdentityParser[String])
			}
		}
		it ("partitionedKeyDef (isDefinedAt)") {
			val builder = partitionedKeyDef[Int, Int, String, Seq[Int], Int](
				new PrimitiveSeqBuilder[Int],
				{case ParserRetVal.Complex(x) => Right(x.sum)},
				{(a,b) => a + " " + b}
			)
			assertResult(Right("z 6")){
				builder.apply("z", Seq(1,2,3), new PrimitiveSeqParser[Int])
			}
		}
		it ("partitionedKeyDef (not isDefinedAt)") {
			val builder = partitionedKeyDef[Int, Int, String, Seq[Int], Int](
				new PrimitiveSeqBuilder[Int],
				{case ParserRetVal.Primitive(x) => Right(x)},
				{(a,b) => a + " " + b}
			)
			assertResult(Left("Unexpected value: Vector(1, 2, 3)", 0)){
				builder.apply("z", Seq(1,2,3), new PrimitiveSeqParser[Int])
			}
		}
		it ("partitionedKeyDef (not isDefinedAt; throwBuilder)") {
			val builder = partitionedKeyDef[Int, Int, String, Any, Int](
				new ThrowBuilder[Int, Int],
				{case ParserRetVal.Primitive(x) => Right(x)},
				{(a,b) => a + " " + b}
			)
			assertResult(Left("using ThrowBuilder::apply", 0)){
				builder.apply("z", Seq(1,2,3), new PrimitiveSeqParser[Int])
			}
		}
		it ("partitionedKeyDef (isDefinedAt; left)") {
			val builder = partitionedKeyDef[Int, Int, String, Seq[Int], Int](
				new PrimitiveSeqBuilder[Int],
				{case ParserRetVal.Complex(x) => Left("Error", 0)},
				{(a,b) => a + " " + b}
			)
			assertResult(Left("Error", 0)){
				builder.apply("z", Seq(1,2,3), new PrimitiveSeqParser[Int])
			}
		}
		it ("partitionedComplexKeyDef (Complex)") {
			val builder = partitionedComplexKeyDef[Int, Int, String, Seq[Int]](
				new PrimitiveSeqBuilder[Int],
				{(a,b) => Right(a + " " + b)}
			)
			assertResult(Right("z Vector(1, 2, 3)")){
				builder.apply("z", Vector(1,2,3), new PrimitiveSeqParser[Int])
			}
		}
		it ("partitionedComplexKeyDef (Primitive)") {
			val builder = partitionedComplexKeyDef[Int, Int, String, Seq[Int]](
				new PrimitiveSeqBuilder[Int],
				{(a,b) => Right(a + " " + b)}
			)
			assertResult(Left("Unexpected value: 5", 0)){
				builder.apply("z", 5, new IdentityParser[Int])
			}
		}
		it ("partitionedComplexKeyDef (Throw)") {
			val builder = partitionedComplexKeyDef[Int, Int, String, Any](
				new ThrowBuilder[Int, Int],
				{(a,b) => Right(a + " " + b)}
			)
			assertResult(Left("using ThrowBuilder::apply", 0)){
				builder.apply("z", Vector(1,2,3), new PrimitiveSeqParser[Int])
			}
		}
	}
}


object PiecewiseBuilderTest {
	case class Person(val name:String, val age:Int)
}
