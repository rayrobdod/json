// this can be run in the scala interpreter, if this library is in the classpath.
// Easiest way to run the interpreter with this library in the class path is probably to run "sbt console" in this project's root directory


// imports from this library;
import com.rayrobdod.json.parser.{Parser, PiecewiseParser, IdentityParser, PrimitiveSeqParser}
import com.rayrobdod.json.builder.{Builder, PrettyJsonBuilder}
import com.rayrobdod.json.union.{StringOrInt, JsonValue, ParserRetVal}

// the data classes
case class Name(given:String, middles:Seq[String], family:String)
case class Person(n:Name, gender:String, isDead:Boolean, interests:Seq[String])

// the data to serialize
val data = Person(Name("Anon", Seq("N", "Y"), "Mouse"), "Undecided", false, Seq("Cheese", "Chess"))

// Example directly subclassing Parser
object NameParser extends Parser[StringOrInt, JsonValue, Nothing, Name] {
  override def parse[A, BF](builder:Builder[StringOrInt, JsonValue, BF, A], input:Name):ParserRetVal[A, Nothing, Nothing, BF] = {
    val a = builder.init
//    for (
//      b <- builder.apply(a, "first", input.given, new IdentityParser[String].mapValue[JsonValue]).complex;
//      c <- builder.apply(b, "middles", input.middles, new PrimitiveSeqParser[String].mapValue[JsonValue].mapKey[StringOrInt]).complex;
//      d <- builder.apply(c, "last", input.family, new IdentityParser[String].mapValue[JsonValue]).complex
//    ) yield {
//      d
//    }
    builder.apply(a, "first", input.given, new IdentityParser[String].mapValue[JsonValue]).complex.flatMap{b:A =>
      builder.apply(b, "middles", input.middles, new PrimitiveSeqParser[String].mapValue[JsonValue].mapKey[StringOrInt]).complex.flatMap{c:A =>
        builder.apply(c, "last", input.family, new IdentityParser[String].mapValue[JsonValue])
      }
    }
  }
}

// Example with PiecewiseParser
val PersonParser = new PiecewiseParser[StringOrInt, JsonValue, Person](
    PiecewiseParser.complexKeyDef("name", {x:Person => x.n}, NameParser)
  , PiecewiseParser.primitiveKeyDef("gender", {x:Person => x.gender})
  , PiecewiseParser.primitiveKeyDef("isAlive", {x:Person => ! x.isDead})
  , PiecewiseParser.complexKeyDef("interests", {x:Person => x.interests}, new PrimitiveSeqParser[String].mapValue[JsonValue].mapKey[StringOrInt])
)

val builder = new PrettyJsonBuilder(new PrettyJsonBuilder.IndentPrettyParams("  ", "\n"))

val result = PersonParser.parse(builder, data)

// at this point, do something with `result`. Probably write to a file given this builder
result.fold({json =>
  System.out.println("Success")
  System.out.println(json)
},{x =>
  System.out.println("Parsed primitive: " + x)
},{x =>
  System.out.println(s"Error $x")
},{x =>
  System.out.println(s"Error $x")
})
