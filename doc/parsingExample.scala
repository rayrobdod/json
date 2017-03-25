// this can be run in the scala interpreter, if this library is in the classpath.
// Easiest way to run the interpreter with this library in the class path is probably to run "sbt console" in this project's root directory


// imports from this library;
import com.rayrobdod.json.parser.{Parser, JsonParser}
import com.rayrobdod.json.builder.{Builder, PiecewiseBuilder, PrimitiveSeqBuilder}
import com.rayrobdod.json.union.{StringOrInt, JsonValue, ParserRetVal}
import com.rayrobdod.json.union.ParserRetVal.{Complex, Primitive, ParserFailure, BuilderFailure}
import com.rayrobdod.json.union.PiecewiseBuilderFailures
import com.rayrobdod.json.union.Failures.{UnsuccessfulTypeCoersion, UnknownKey}

// the data classes
case class Name(given:String, middle:String, family:String)
case class Person(n:Name, gender:String, isDead:Boolean, interests:Seq[String])

// the json data to parse- presumably in a real system this would be read from a file rather than hardcoded
val json = """{
    "name":{
      "given":"Raymond",
      "middle":"Robert",
      "family":"Dodge"
    },
    "gender":"male",
    "isAlive":true,
    "interests":["bowling", "tennis", "programming", "twitch plays pokémon"]
}"""

// Example directly subclassing Builder
object NameBuilder extends Builder[StringOrInt, JsonValue, PiecewiseBuilderFailures, Name] {
  override type Middle = Name
  override def init:Name = Name("", "", "")
  override def apply[Input, PF](folding:Name, key:StringOrInt, input:Input, parser:Parser[StringOrInt, JsonValue, PF, Input]):ParserRetVal[Name, Nothing, PF, PiecewiseBuilderFailures] = {
    // we only expect strings, so might as well parse the value at the beginning
    parser.parsePrimitive(input).flatMap{value:JsonValue => value.stringToEither{strValue:String =>
      key match {
        case StringOrInt.Left("given") => Right(folding.copy(given = strValue))
        case StringOrInt.Left("middle") => Right(folding.copy(middle = strValue))
        case StringOrInt.Left("family") => Right(folding.copy(family = strValue))
        case x => Left(UnknownKey)
      }
    }.fold({err => BuilderFailure(err)}, {x => Complex(x)})}
  }
  override def finalize(x:Middle) = ParserRetVal.Complex(x)
}

// example using PiecewiseBuilder
val PersonBuilder = {
  new PiecewiseBuilder[StringOrInt, JsonValue, Person](Person(Name("", "", ""), "", false, Seq.empty))
    // paritioned complex key def
    .addDef("name", PiecewiseBuilder.partitionedComplexKeyDef[StringOrInt, JsonValue, Person, Name](
      NameBuilder,
      {(folding, name) => Complex(folding.copy(n = name))}
    ))
    // paritioned private key def
    .addDef("gender", PiecewiseBuilder.partitionedPrimitiveKeyDef[StringOrInt, JsonValue, Person, String](
      {case x => x.stringToEither{x:String => Right(x)}.fold({err => BuilderFailure(err)}, {x => Complex(x)})},
      {(folding,x) => folding.copy(gender = x)}
    ))
    // raw private key def
    .addDef("isAlive", new PiecewiseBuilder.KeyDef[StringOrInt, JsonValue, Person]{
      override def apply[Input, PF](folding:Person, input:Input, parser:Parser[StringOrInt, JsonValue, PF, Input]):ParserRetVal[Person, Nothing, PF, PiecewiseBuilderFailures] = {
        parser.parsePrimitive(input)
        	.flatMap{x:JsonValue => x.booleanToEither{b => Right(folding.copy(isDead = !b))}
        		.fold({x => BuilderFailure(UnsuccessfulTypeCoersion("","",""))}, {x => Complex(x)})
        	}
      }
    })
    // raw complex key def
    .addDef("interests", new PiecewiseBuilder.KeyDef[StringOrInt, JsonValue, Person]{
      override def apply[Input, PF](folding:Person, input:Input, parser:Parser[StringOrInt, JsonValue, PF, Input]):ParserRetVal[Person, Nothing, PF, PiecewiseBuilderFailures] = {
        parser.parse(new PrimitiveSeqBuilder, input) match {
          case ParserRetVal.Complex(seq) => {
            seq.foldLeft[ParserRetVal[Vector[String], Nothing, PF, PiecewiseBuilderFailures]](Complex(Vector.empty)){(folding:ParserRetVal[Vector[String], Nothing, PF, PiecewiseBuilderFailures], value:JsonValue) => folding.complex.flatMap(sequence => value match {
              case JsonValue.JsonValueString(s) => Complex(sequence :+ s)
              case x => BuilderFailure(UnsuccessfulTypeCoersion(x, "JsonValue", "String"))
            })}.complex.map{x => folding.copy(interests = x)}
          }
          // allow a single interest to be represented as a string not in an array
          case ParserRetVal.Primitive(JsonValue.JsonValueString(interest)) => Complex(folding.copy(interests = Seq(interest)))
          case unknown => BuilderFailure(UnsuccessfulTypeCoersion(unknown, "Any", "Seq[String]"))
        }
    }})
}

// parse the json file
val result:ParserRetVal[Person, JsonValue, JsonParser.Failures, PiecewiseBuilderFailures] = new JsonParser().parse(PersonBuilder, json)

// at this point, do something with `result`
result.fold({person =>
  System.out.println("Success")
  System.out.println(person)
},{x =>
  System.out.println("Parsed primitive: " + x)
},{x =>
  System.out.println(s"Json formatted incorrectly: $x")
},{x =>
  System.out.println(s"Json contents doesn't match expected model: $x")
})
