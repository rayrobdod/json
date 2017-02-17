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
object NameParser extends Parser[StringOrInt, JsonValue, Name] {
  override def parse[A](builder:Builder[StringOrInt, JsonValue, A], input:Name):ParserRetVal[A, JsonValue] = {
    val a = builder.init
    val e = for (
      b <- builder.apply(a, "first", input.given, new IdentityParser[String].mapValue[JsonValue]).right;
      c <- builder.apply(b, "middles", input.middles, new PrimitiveSeqParser[String].mapValue[JsonValue].mapKey[StringOrInt]).right;
      d <- builder.apply(c, "last", input.family, new IdentityParser[String].mapValue[JsonValue]).right
    ) yield {
      d
    }
    e.fold(
      {si => ParserRetVal.Failure(si._1, si._2)},
      {res => ParserRetVal.Complex(res)}
    )
  }
}

// Example with PiecewiseParser
import PiecewiseParser.KeyDefSyntax
val PersonParser = new PiecewiseParser[StringOrInt, JsonValue, Person](
    "name" valueIs ({x:Person => x.n}, NameParser)
  , "gender" valueIs {x => x.gender}
  , "isAlive" valueIs {x => ! x.isDead}
  , "interests" valueIs ({x => x.interests}, new PrimitiveSeqParser[String].mapValue[JsonValue].mapKey[StringOrInt])
)

val builder = new PrettyJsonBuilder(new PrettyJsonBuilder.IndentPrettyParams("  ", "\n"))

val result = PersonParser.parse(builder, data)

// at this point, do something with `result`. Probably write to a file given this builder
result.fold({json =>
  System.out.println("Success")
  System.out.println(json)
},{x =>
  System.out.println("Parsed primitive: " + x)
},{(msg, idx) =>
  System.out.println(s"Parse error at char $idx: $msg")
})
