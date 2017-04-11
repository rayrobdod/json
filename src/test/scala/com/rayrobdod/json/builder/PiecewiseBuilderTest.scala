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
import org.scalatest.FunSpec;
import com.rayrobdod.json.union.{StringOrInt, JsonValue, ParserRetVal}
import com.rayrobdod.json.union.ParserRetVal.{Complex, BuilderFailure}
import com.rayrobdod.json.parser.{Parser, IdentityParser, PrimitiveSeqParser}
import com.rayrobdod.json.builder.PiecewiseBuilder.Failures
import com.rayrobdod.json.builder.PiecewiseBuilder.Failures.{
		ExpectedPrimitive, ExpectedComplex, UnknownKey, UnsuccessfulTypeCoercion
}
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
			assertResult(Complex(new Person(name, 0))){
				new PiecewiseBuilder(new Person("", 0))
						.addDef("name", new KeyDef[String, String, Person]{
							def apply[I, PF, EX](s:Person, i:I, p:Parser[String, String, PF, EX, I], ex:EX) = {
								p.parsePrimitive(i, ExpectedPrimitive)
									.flatMap{x:String => Complex(s.copy(name = x))}
							}
						})
						.apply(new Person("", 0), "name", name, new IdentityParser[String], ())
			}
		}
		it ("Acts upon provided keydef (2)") {
			val age = 9001
			assertResult(Complex(new Person("", age))){
				new PiecewiseBuilder(new Person("", 0))
						.addDef("age", new KeyDef[String, Int, Person]{
							def apply[I, PF, EX](s:Person, i:I, p:Parser[String, Int, PF, EX, I], ex:EX) = {
								p.parsePrimitive(i, ExpectedPrimitive)
									.flatMap{x:Int => Complex(s.copy(age = x))}
							}
						})
						.apply(new Person("", 0), "age", age, new IdentityParser[Int], ())
			}
		}
		it ("Acts upon provided keydef (partitionedPrimitive)") {
			val age = 9001
			assertResult(Complex(new Person("", age))){
				new PiecewiseBuilder(new Person("", 0))
						.addDef("age", PiecewiseBuilder.partitionedPrimitiveKeyDef[String, Int, Person, Int]({case x:Int => x}, {(f,i) => f.copy(age = i)}))
						.apply(new Person("", 0), "age", age, new IdentityParser[Int], ())
			}
		}
		it ("Throws excpetion on unknown key") {
			assertResult(BuilderFailure(UnknownKey, ())){
				new PiecewiseBuilder[String, String, Person](new Person("", 0))
						.apply(new Person("", 0), "asdfjkl;", "hello", new IdentityParser[String], ())
			}
		}
		it ("ignores unknown key after call to ignoreUnknownKeys") {
			assertResult(Complex(new Person("", 0))){
				new PiecewiseBuilder[String, String, Person](new Person("", 0)).ignoreUnknownKeys
						.apply(new Person("", 0), "asdfjkl;", "hello", new IdentityParser[String], ())
			}
		}
	}
	
	describe("PiecewiseBuilder + JsonParser") {
		import com.rayrobdod.json.parser.JsonParser
		
		it ("works") {
			val builder = new PiecewiseBuilder[StringOrInt, JsonValue, Person](new Person("", 0))
				.addDef("name", new KeyDef[StringOrInt, JsonValue, Person]{
					def apply[I, PF, EX](s:Person, i:I, p:Parser[StringOrInt, JsonValue, PF, EX, I], ex:EX) = {
						p.parsePrimitive[Failures](i, ExpectedPrimitive).flatMap{x:JsonValue =>
							x.stringToEither{x:String => Right(s.copy(name = x))}.fold({BuilderFailure(_, ex)}, {Complex(_)})
						}
					}
				})
				.addDef("age", new KeyDef[StringOrInt, JsonValue, Person]{
					def apply[I, PF, EX](s:Person, i:I, p:Parser[StringOrInt, JsonValue, PF, EX, I], ex:EX) = {
						p.parsePrimitive(i, ExpectedPrimitive).flatMap{x:JsonValue =>
							x.integerToEither{x:Int => Right(s.copy(age = x))}.fold({BuilderFailure(_, ex)}, {Complex(_)})
						}
					}
				})
			
			assertResult(Complex(Person("nqpppnl",1))){
				new JsonParser().parse(builder, 
					"""{"name":"nqpppnl","age":1}"""
				)
			}
		}
		it ("nested") {
			val exp = Seq(Person("a", 5), Person("b", 6))
			
			val personBuilder = new PiecewiseBuilder[StringOrInt, JsonValue, Person](new Person("", 0))
				.addDef("name", new KeyDef[StringOrInt, JsonValue, Person]{
					def apply[I, PF, EX](s:Person, i:I, p:Parser[StringOrInt, JsonValue, PF, EX, I], ex:EX) = {
						p.parsePrimitive[Failures](i, ExpectedPrimitive).flatMap{x:JsonValue =>
							x.stringToEither{x:String => Right(s.copy(name = x))}.fold({BuilderFailure(_, ex)}, {Complex(_)})
						}
					}
				})
				.addDef("age", new KeyDef[StringOrInt, JsonValue, Person]{
					def apply[I, PF, EX](s:Person, i:I, p:Parser[StringOrInt, JsonValue, PF, EX, I], ex:EX) = {
						p.parsePrimitive(i, ExpectedPrimitive).flatMap{x:JsonValue =>
							x.integerToEither{x:Int => Right(s.copy(age = x))}.fold({BuilderFailure(_, ex)}, {Complex(_)})
						}
					}
				})
			
			val seqBuilder = new PiecewiseBuilder[StringOrInt, JsonValue, Seq[Person]](
				Nil,
				new KeyDef[StringOrInt, JsonValue, Seq[Person]]{
					def apply[I, PF, EX](s:Seq[Person], i:I, p:Parser[StringOrInt, JsonValue, PF, EX, I], ex:EX) = {
						p.parse(personBuilder, i)
								.complex.map{s :+ _}
								.primitive.flatMap{x => BuilderFailure(ExpectedComplex, ex)}
					}
				}
			)
				
			assertResult(Complex(exp)){
				new JsonParser().parse(seqBuilder, 
					"""[{"name":"a","age":5},{"name":"b","age":6}]"""
				)
			}
		}
	}
	
	describe("KeyDefs") {
		import PiecewiseBuilder._
		
		object Folding
		object Value
		object Extra
		object Out1
		
		
		describe ("ignoreKeyDef") {
			it ("echos the folding value") {
				assert(
					Complex(Folding) == ignoreKeyDef.apply(Folding, Value, new IdentityParser[Value.type], Extra)
				)
			}
		}
		describe ("throwKeyDef") {
			it ("returns a UnknownKey builder failure") {
				assert(
					BuilderFailure(UnknownKey, Extra) == throwKeyDef.apply(Folding, Value, new IdentityParser[Value.type], Extra)
				)
			}
		}
		describe ("partitionedPrimitiveKeyDef") {
			
			
			it ("if convert.isDefinedAt, return a Complex with the mapped values") {
				val builder = partitionedPrimitiveKeyDef[Any, String, Option[Int], Int]({case "abc" => 3}, {(a,b) => Some(b)})
				assertResult(Complex(Some(3))){
					builder.apply(None, "abc", new IdentityParser[String], "def")
				}
			}
			it ("partitionedPrimitiveKeyDef (not isDefinedAt)") {
				val builder = partitionedPrimitiveKeyDef[Any, String, Option[Int], Int]({case "abc" => 3}, {(a,b) => Some(b)})
				assertResult(BuilderFailure(UnsuccessfulTypeCoercion, "def")){
					builder.apply(None, "asdf", new IdentityParser[String], "def")
				}
			}
		}
		it ("partitionedKeyDef (isDefinedAt)") {
			val builder = partitionedKeyDef[Int, Int, String, Seq[Int], Int](
				new PrimitiveSeqBuilder[Int, ExpectedPrimitive.type]( ExpectedPrimitive ),
				{case ParserRetVal.Complex(x) => Complex(x.sum)},
				{(a,b) => a + " " + b}
			)
			assertResult(Complex("z 6")){
				builder.apply("z", Seq(1,2,3), new PrimitiveSeqParser[Int], "def")
			}
		}
		it ("partitionedKeyDef (not isDefinedAt)") {
			val builder = partitionedKeyDef[Int, Int, String, Seq[Int], Int](
				new PrimitiveSeqBuilder[Int, ExpectedPrimitive.type]( ExpectedPrimitive ),
				{case ParserRetVal.Primitive(x) => Complex(x)},
				{(a,b) => a + " " + b}
			)
			assertResult(BuilderFailure(UnsuccessfulTypeCoercion, "def")){
				builder.apply("z", Seq(1,2,3), new PrimitiveSeqParser[Int], "def")
			}
		}
		it ("partitionedKeyDef (not isDefinedAt; throwBuilder)") {
			val builder = partitionedKeyDef[Int, Int, String, Any, Int](
				PiecewiseBuilderTest.throwBuilder2,
				{case ParserRetVal.Primitive(x) => Complex(x)},
				{(a,b) => a + " " + b}
			)
			assertResult(BuilderFailure(UnknownKey, ())){
				builder.apply("z", Seq(1,2,3), new PrimitiveSeqParser[Int], "def")
			}
		}
		it ("partitionedKeyDef (isDefinedAt; left)") {
			val builder = partitionedKeyDef[Int, Int, String, Seq[Int], Int](
				new PrimitiveSeqBuilder[Int, ExpectedPrimitive.type]( ExpectedPrimitive ),
				{case ParserRetVal.Complex(x) => BuilderFailure(UnknownKey, ())},
				{(a,b) => a + " " + b}
			)
			assertResult(BuilderFailure(UnknownKey, "def")){
				builder.apply("z", Seq(1,2,3), new PrimitiveSeqParser[Int], "def")
			}
		}
		it ("partitionedComplexKeyDef (Complex)") {
			val builder = partitionedComplexKeyDef[Int, Int, String, Seq[Int]](
				new PrimitiveSeqBuilder[Int, ExpectedPrimitive.type]( ExpectedPrimitive ),
				{(a,b) => Complex(a + " " + b)}
			)
			assertResult(Complex("z Vector(1, 2, 3)")){
				builder.apply("z", Vector(1,2,3), new PrimitiveSeqParser[Int], "def")
			}
		}
		it ("partitionedComplexKeyDef (Primitive)") {
			val builder = partitionedComplexKeyDef[Int, Int, String, Seq[Int]](
				new PrimitiveSeqBuilder[Int, ExpectedPrimitive.type]( ExpectedPrimitive ),
				{(a,b) => Complex(a + " " + b)}
			)
			assertResult(BuilderFailure(ExpectedComplex, "def")){
				builder.apply("z", 5, new IdentityParser[Int], "def")
			}
		}
		it ("partitionedComplexKeyDef (Throw)") {
			val builder = partitionedComplexKeyDef[Int, Int, String, Any](
				PiecewiseBuilderTest.throwBuilder2,
				{(a,b) => Complex(a + " " + b)}
			)
			assertResult(BuilderFailure(UnknownKey, ())){
				builder.apply("z", Vector(1,2,3), new PrimitiveSeqParser[Int], "def")
			}
		}
	}
}


object PiecewiseBuilderTest {
	case class Person(val name:String, val age:Int)
	
	def throwBuilder2 = new ThrowBuilder[Failures](UnknownKey).mapResult[Any]{x => x}
}
