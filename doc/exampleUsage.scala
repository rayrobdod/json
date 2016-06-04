// this can be run in the scala interpreter, if this library is in the classpath.
// Easiest way to run the interpreter with this library in the class path is probably to run "sbt console" in this project's root directory


// other imports
import scala.util.{Try, Success, Failure}
import scala.util.{Either, Left, Right}
import java.text.ParseException

// imports from this library;
import com.rayrobdod.json.parser.{Parser, JsonParser}
import com.rayrobdod.json.builder.{Builder, BuildableBuilder, PrimitiveSeqBuilder, ThrowBuilder}
import com.rayrobdod.json.union.{StringOrInt, JsonValue}

// the data classes
case class Name(given:String, middle:String, family:String)
case class Person(n:Name, gender:String, isDead:Boolean, interests:Seq[String])

// the json data to parse- presumably in a real system this would be read from a file rather than direct
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
  override def apply[Input](folding:Name, key:StringOrInt, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):Try[Name] = {
    // we only expect strings, so might as well parse the value here
    parser.parse(new ThrowBuilder, input).flatMap{value:Either[Any, JsonValue] =>
      ((key, value)) match {
        case ((StringOrInt.Left("given"), Right(JsonValue.JsonValueString(x)))) => Try(folding.copy(given = x))
        case ((StringOrInt.Left("middle"), Right(JsonValue.JsonValueString(x)))) => Try(folding.copy(middle = x))
        case ((StringOrInt.Left("family"), Right(JsonValue.JsonValueString(x)))) => Try(folding.copy(family = x))
        case x => Failure(new ParseException("Unexpected key/value for name: " + x, -1))
      }
    }
  }
}

final class ASDF[BuilderResult, ZXCV](
    builder:Builder[StringOrInt, JsonValue, BuilderResult],
    convert:PartialFunction[Either[BuilderResult, JsonValue], Try[ZXCV]],
    fold:Function2[Person, ZXCV, Person]
) extends BuildableBuilder.KeyDef[StringOrInt, JsonValue, Person] {
  def apply[Input](folding:Person, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):Try[Person] = {
    parser.parse(builder, input).flatMap{convert.orElse{case x => Failure(new ParseException("Unexpected value: " + x, -1))}}.map{x => fold(folding, x)}
  }
}

// example using BuildableBuilder
val PersonBuilder = {
  new BuildableBuilder[StringOrInt, JsonValue, Person](Person(Name("", "", ""), "", false, Seq.empty))
    .addDef("name", new ASDF[Name, Name](NameBuilder, {case Left(name) => Try(name)}, {(folding, name) => folding.copy(n = name)}))
    .addDef("gender", new ASDF[Any, String](new ThrowBuilder, {case Right(JsonValue.JsonValueString(g)) => Try(g)}, {(folding,x) => folding.copy(gender = x)}))
    .addDef("isDead", new BuildableBuilder.KeyDef[StringOrInt, JsonValue, Person]{ override def apply[Input](folding:Person, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):Try[Person] = {
      parser.parse(new ThrowBuilder, input).flatMap{_ match { case Right(JsonValue.JsonValueBoolean(b)) => Try(folding.copy(isDead = b)); case unknown => Failure(new ParseException("isDead not boolean: " + unknown, -1))}}
    }})
    .addDef("interests", new BuildableBuilder.KeyDef[StringOrInt, JsonValue, Person]{ override def apply[Input](folding:Person, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):Try[Person] = {
      parser.parse(new PrimitiveSeqBuilder, input).flatMap{_ match {
        case Left(seq) => {
          seq.foldLeft[Try[Vector[String]]](Success(Vector.empty)){(folding:Try[Vector[String]], value:JsonValue) => folding.flatMap(sequence => value match {
            case JsonValue.JsonValueString(s) => Try(sequence :+ s)
            case _ => Failure(new ParseException("interests not a seq of strings", -1))
          })}.map{x => folding.copy(interests = x)}
        }
        case unknown => Failure(new ParseException("interests not Seq: " + unknown, -1))
      }}
    }})
}

// parse the json file
val p:Person = new JsonParser().parse(PersonBuilder, json).get.left.get

// at this point, do something with p
System.out.println(p)
