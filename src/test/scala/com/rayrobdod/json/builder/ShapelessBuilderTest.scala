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
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.CborValue
import com.rayrobdod.json.union.CborValue._
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.parser.MapParser

class ShapelessBuilderTest extends FunSpec {
	case class Name(given:String, middle:String, family:String)
	case class IceCream(flavor:String, scoops:Int, waffleCone:Boolean)
	case class Record(holder:Name, description:String)
	case class StringList(vals:Seq[String])
	val emptyName:Name = Name("", "", "")
	val emptyIceCream:IceCream = IceCream("", 0, false)
	val emptyRecord:Record = Record(emptyName, "")
	
	
	describe("ShapelessBuilder") {
		describe("of Name from String") {
			val builder = new ShapelessBuilder[String, Name](emptyName)
			
			it ("finds implicit empty name for init") {
				assertResult(emptyName){builder.init}
			}
			it ("accepts the key \"Given\"") {
				assertResult(Right(emptyName.copy(given = "First")))
					{builder.apply(emptyName, "given", "First", new IdentityParser[String])}
			}
			it ("accepts the key \"Family\"") {
				assertResult(Right(emptyName.copy(family = "First")))
					{builder.apply(emptyName, "family", "First", new IdentityParser[String])}
			}
			it ("rejects the key \"Apple\"") {
				assertResult(Left("Unexpected key: Apple", 0))
					{builder.apply(emptyName, "Apple", "First", new IdentityParser[String])}
			}
		}
		describe("of Name from JsonValue") {
			val builder = new ShapelessBuilder[JsonValue, Name](emptyName)
			
			it ("finds implicit empty name for init") {
				assertResult(emptyName){builder.init}
			}
			it ("accepts the key \"Given\"") {
				assertResult(Right(emptyName.copy(given = "First")))
					{builder.apply(emptyName, "given", "First":JsonValue, new IdentityParser[JsonValue])}
			}
			it ("accepts the key \"Family\"") {
				assertResult(Right(emptyName.copy(family = "First")))
					{builder.apply(emptyName, "family", "First":JsonValue, new IdentityParser[JsonValue])}
			}
			it ("rejects an int value") {
				assertResult(Left("Expected string", 0))
					{builder.apply(emptyName, "family", JsonValue(23), new IdentityParser[JsonValue])}
			}
		}
		describe("of IceCream from JsonValue") {
			val builder = new ShapelessBuilder[JsonValue, IceCream](emptyIceCream)
			
			it ("finds implicit empty name for init") {
				assertResult(emptyIceCream){builder.init}
			}
			it ("accepts a string Key with \"flavor\"") {
				assertResult(Right(emptyIceCream.copy(flavor = "Chocolate")))
					{builder.apply(emptyIceCream, "flavor", JsonValue("Chocolate"), new IdentityParser[JsonValue])}
			}
			it ("accepts a int Key with \"scoops\"") {
				assertResult(Right(emptyIceCream.copy(scoops = 2)))
					{builder.apply(emptyIceCream, "scoops", JsonValue(2), new IdentityParser[JsonValue])}
			}
			it ("accepts a boolean Key with \"waffleCone\"") {
				assertResult(Right(emptyIceCream.copy(waffleCone = true)))
					{builder.apply(emptyIceCream, "waffleCone", JsonValue(true), new IdentityParser[JsonValue])}
			}
			it ("rejects an int value forthe 'flavor' key") {
				assertResult(Left("Expected string", 0))
					{builder.apply(emptyIceCream, "flavor", JsonValue(31), new IdentityParser[JsonValue])}
			}
			it ("rejects an double value for the 'scoops' key") {
				assertResult(Left("Expected Int: 3.1415", 0))
					{builder.apply(emptyIceCream, "scoops", JsonValue(3.1415), new IdentityParser[JsonValue])}
			}
			it ("rejects key 'asfdadf'") {
				assertResult(Left("Unexpected key: asfdadf", 0))
					{builder.apply(emptyIceCream, "asfdadf", JsonValue(31), new IdentityParser[JsonValue])}
			}
		}
		describe("of IceCream from CborValue") {
			val builder = new ShapelessBuilder[CborValue, IceCream](emptyIceCream)
			
			it ("finds implicit empty name for init") {
				assertResult(emptyIceCream){builder.init}
			}
			it ("accepts a string Key with \"flavor\"") {
				assertResult(Right(emptyIceCream.copy(flavor = "Chocolate")))
					{builder.apply(emptyIceCream, "flavor", CborValue("Chocolate"), new IdentityParser[CborValue])}
			}
			it ("accepts a int Key with \"scoops\"") {
				assertResult(Right(emptyIceCream.copy(scoops = 2)))
					{builder.apply(emptyIceCream, "scoops", CborValue(2), new IdentityParser[CborValue])}
			}
			it ("accepts a boolean Key with \"waffleCone\"") {
				assertResult(Right(emptyIceCream.copy(waffleCone = true)))
					{builder.apply(emptyIceCream, "waffleCone", CborValue(true), new IdentityParser[CborValue])}
			}
			it ("rejects an int value forthe 'flavor' key") {
				assertResult(Left("Expected string", 0))
					{builder.apply(emptyIceCream, "flavor", CborValue(31), new IdentityParser[CborValue])}
			}
			it ("rejects an double value for the 'scoops' key") {
				assertResult(Left("Expected integral number", 0))
					{builder.apply(emptyIceCream, "scoops", CborValue(3.1415), new IdentityParser[CborValue])}
			}
			it ("rejects key 'asfdadf'") {
				assertResult(Left("Unexpected key: asfdadf", 0))
					{builder.apply(emptyIceCream, "asfdadf", CborValue(31), new IdentityParser[CborValue])}
			}
		}
		describe("of Record from String") {
			val builder = {
				implicit val nameBuilder = new ShapelessBuilder[String, Name](emptyName)
				new ShapelessBuilder[String, Record](emptyRecord)
			}
			
			it ("finds implicit empty record for init") {
				assertResult(emptyRecord){builder.init}
			}
			it ("accepts the key \"Holder\"") {
				assertResult(Right(emptyRecord.copy(holder = Name("First", "", "Last"))))
					{builder.apply(emptyRecord, "holder", Map("given" -> "First", "family" -> "Last"), new MapParser[String, String])}
			}
		}
		describe("of StringList from String") {
			implicit val seqBuilder = new PrimitiveSeqBuilder[String]
			val builder = new ShapelessBuilder[String, StringList](new StringList(Nil))
			
			it ("accepts the key \"vals\"") {
				assertResult(Right(new StringList(Seq("a", "b", "c"))))
					{builder.apply(new StringList(Nil), "vals", Map("0" -> "a", "1" -> "b", "2" -> "c"), new MapParser[String, String])}
			}
		}
	}
	
	describe("ShapelessBuilder integration") {
		import com.rayrobdod.json.parser.{JsonParser, CborParser}
		
		it ("ShapelessBuilder + JsonParser + primitives") {
			assertResult(Name("Homer", "Jay", "Simpson")){
				new JsonParser().parse(
					new ShapelessBuilder[JsonValue, Name](emptyName).mapKey[StringOrInt](StringOrInt.unwrapToString _),
					"""{"given":"Homer", "family":"Simpson", "middle":"Jay"}"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
	}
}
