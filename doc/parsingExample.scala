// this can be run in the scala interpreter, if this library is in the classpath.
// Easiest way to run the interpreter with this library in the class path is probably to run "sbt console" in this project's root directory


// imports from this library;
import com.rayrobdod.json.parser.{Parser, JsonParser}
import com.rayrobdod.json.builder.{Builder, PiecewiseBuilder, PrimitiveSeqBuilder}
import com.rayrobdod.json.union.{StringOrInt, JsonValue, ParserRetVal}
import com.rayrobdod.json.union.ParserRetVal.{Complex, Failure}

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
object NameBuilder extends Builder[StringOrInt, JsonValue, Name] {
  override def init:Name = Name("", "", "")
  override def apply[Input](folding:Name, key:StringOrInt, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):ParserRetVal[Name, Nothing] = {
    // we only expect strings, so might as well parse the value at the beginning
    parser.parsePrimitive(input).flatMap{value:JsonValue => value.stringToEither{strValue:String =>
      key match {
        case StringOrInt.Left("given") => Right(folding.copy(given = strValue))
        case StringOrInt.Left("middle") => Right(folding.copy(middle = strValue))
        case StringOrInt.Left("family") => Right(folding.copy(family = strValue))
        case x => Left("NameBuilder: Unexpected key/value: " + x, 0)
      }
    }.fold({si => Failure(si._1, si._2)}, {x => Complex(x)})}.mergeToComplex
  }
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
      {case JsonValue.JsonValueString(g) => Complex(g)},
      {(folding,x) => folding.copy(gender = x)}
    ))
    // raw private key def
    .addDef("isAlive", new PiecewiseBuilder.KeyDef[StringOrInt, JsonValue, Person]{
      override def apply[Input](folding:Person, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):ParserRetVal[Person, Nothing] = {
        parser.parsePrimitive(input)
        	.flatMap{x:JsonValue => x.booleanToEither{b => Right(folding.copy(isDead = !b))}
        		.fold({x => Failure(x._1, x._2)}, {x => Complex(x)})
        	}
        	.mergeToComplex
      }
    })
    // raw complex key def
    .addDef("interests", new PiecewiseBuilder.KeyDef[StringOrInt, JsonValue, Person]{
      override def apply[Input](folding:Person, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):ParserRetVal[Person, Nothing] = {
        parser.parse(new PrimitiveSeqBuilder, input) match {
          case ParserRetVal.Complex(seq) => {
            seq.foldLeft[ParserRetVal[Vector[String], Nothing]](Complex(Vector.empty)){(folding:ParserRetVal[Vector[String], Nothing], value:JsonValue) => folding.complex.flatMap(sequence => value match {
              case JsonValue.JsonValueString(s) => Complex(sequence :+ s)
              case _ => Failure("interests is seq, but contained non-strings", 0)
            })}.complex.map{x => folding.copy(interests = x)}
          }
          // allow a single interest to be represented as a string not in an array
          case ParserRetVal.Primitive(JsonValue.JsonValueString(interest)) => Complex(folding.copy(interests = Seq(interest)))
          case unknown => Failure("interests not Seq: " + unknown, 0)
        }
    }})
}

// parse the json file
val result:ParserRetVal[Person, JsonValue] = new JsonParser().parse(PersonBuilder, json)

// at this point, do something with `result`
result.fold({person =>
  System.out.println("Success")
  System.out.println(person)
},{x =>
  System.out.println("Parsed primitive: " + x)
},{(msg, idx) =>
  System.out.println(s"Parse error at char $idx: $msg")
})
