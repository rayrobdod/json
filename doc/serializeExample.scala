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
object NameParser extends Parser[StringOrInt, JsonValue, Nothing, Unit, Name] {
  override def parse[A, BF](builder:Builder[StringOrInt, JsonValue, BF, A], input:Name):ParserRetVal[A, Nothing, Nothing, BF, Unit] = {
    val a = builder.init
    (for (
      b <- builder.apply(a, "first", input.given, IdentityParser[JsonValue, String], ()).complex;
      c <- builder.apply(b, "middles", input.middles, PrimitiveSeqParser[StringOrInt, JsonValue, String], ()).complex;
      d <- builder.apply(c, "last", input.family, IdentityParser[JsonValue, String], ()).complex
    ) yield {
      d
    }).complex.flatMap{builder.finish(())}
  }
}

// Example with PiecewiseParser
import PiecewiseParser.KeyDefSyntax
val PersonParser = new PiecewiseParser[StringOrInt, JsonValue, Person](
    "name" valueIs ({x:Person => x.n}, NameParser)
  , "gender" valueIs {x => x.gender}
  , "isAlive" valueIs {x => ! x.isDead}
  , "interests" valueIs ({x => x.interests}, PrimitiveSeqParser[StringOrInt, JsonValue, String])
)

// val builder = new PrettyJsonBuilder(new PrettyJsonBuilder.IndentPrettyParams("  ", "\n"))
val builder = PrettyJsonBuilder.space2()

val result = PersonParser.parse(builder, data)

// at this point, do something with `result`. Probably write to a file given this builder
result.fold({json =>
  System.out.println("Success")
  System.out.println(json)
},{x =>
  System.out.println("Parsed primitive: " + x)
},{x =>
  System.out.println(s"Error $x")
},{(x, extra:Unit) =>
  System.out.println(s"Error $x")
})
