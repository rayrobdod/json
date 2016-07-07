// this can be run in the scala interpreter, if this library is in the classpath.
// Easiest way to run the interpreter with this library in the class path is probably to run "sbt console" in this project's root directory


// other imports
import scala.util.{Either, Left, Right}
import java.text.ParseException

// imports from this library;
import com.rayrobdod.json.parser.{Parser, JsonParser}
import com.rayrobdod.json.builder.{Builder, BuildableBuilder, PrimitiveSeqBuilder, ThrowBuilder}
import com.rayrobdod.json.union.{StringOrInt, JsonValue, ParserRetVal}

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
    "isDead":false,
    "interests":["bowling", "tennis", "programming", "twitch plays pokémon"]
}"""

// Exaple using subclassing of Builder
object NameBuilder extends Builder[StringOrInt, JsonValue, Name] {
  override def init:Name = Name("", "", "")
  override def apply[Input](folding:Name, key:StringOrInt, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):Either[(String, Int), Name] = {
    // we only expect strings, so might as well parse the value at the beginning
    parser.parsePrimitive(input).right.flatMap{value:JsonValue =>
      ((key, value)) match {
        case ((StringOrInt.Left("given"), JsonValue.JsonValueString(x))) => Right(folding.copy(given = x))
        case ((StringOrInt.Left("middle"), JsonValue.JsonValueString(x))) => Right(folding.copy(middle = x))
        case ((StringOrInt.Left("family"), JsonValue.JsonValueString(x))) => Right(folding.copy(family = x))
        case x => Left("NameBuilder: Unexpected key/value: " + x, 0)
      }
    }
  }
}

// example using BuildableBuilder
val PersonBuilder = {
  new BuildableBuilder[StringOrInt, JsonValue, Person](Person(Name("", "", ""), "", false, Seq.empty))
    // paritioned complex key def
    .addDef("name", BuildableBuilder.partitionedComplexKeyDef[StringOrInt, JsonValue, Person, Name](
      NameBuilder,
      {(folding, name) => Right(folding.copy(n = name))}
    ))
    // paritioned private key def
    .addDef("gender", BuildableBuilder.partitionedPrimitiveKeyDef[StringOrInt, JsonValue, Person, String](
      {case JsonValue.JsonValueString(g) => Right(g)},
      {(folding,x) => folding.copy(gender = x)}
    ))
    // raw private key def
    .addDef("isDead", new BuildableBuilder.KeyDef[StringOrInt, JsonValue, Person]{
      override def apply[Input](folding:Person, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):Either[(String, Int), Person] = {
        parser.parsePrimitive(input) match {
          case Right(JsonValue.JsonValueBoolean(b)) => Right(folding.copy(isDead = b));
          case unknown => Left(("isDead not boolean: " + unknown, 0))
        }
      }
    })
    // raw complex key def
    .addDef("interests", new BuildableBuilder.KeyDef[StringOrInt, JsonValue, Person]{
      override def apply[Input](folding:Person, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):Either[(String, Int), Person] = {
        parser.parse(new PrimitiveSeqBuilder, input) match {
          case ParserRetVal.Complex(seq) => {
            seq.foldLeft[Either[(String, Int),Vector[String]]](Right(Vector.empty)){(folding:Either[(String, Int),Vector[String]], value:JsonValue) => folding.right.flatMap(sequence => value match {
              case JsonValue.JsonValueString(s) => Right(sequence :+ s)
              case _ => Left("interests is seq, but contained non-strings", 0)
            })}.right.map{x => folding.copy(interests = x)}
          }
          // allow a single interest to be represented as a string not in an array
          case ParserRetVal.Primitive(JsonValue.JsonValueString(interest)) => Right(folding.copy(interests = Seq(interest)))
          case unknown => Left(("interests not Seq: " + unknown, 0))
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
