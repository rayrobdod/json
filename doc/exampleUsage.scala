// this can be run in the scala interpreter, if this library is in the classpath.
// Easiest way to run the interpreter with this library in the class path is probably to run "sbt console" in this project's root directory


// other imports
import java.text.ParseException

// imports from this library;
import com.rayrobdod.json.parser.JsonParser;
import com.rayrobdod.json.builder.Builder;

// the data classes
case class Name(given:String, middle:String, family:String)
case class Person(n:Name, gender:String, isDead:Boolean, interests:Set[String])

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

// the classes that extend Builder and convert the data into the data classes
object SetBuilder extends Builder[Set[String]] {
  def init:Set[String] = Set.empty
  def apply(folding:Set[String], key:String, value:Any) = {
    folding + value.toString
  }
  def childBuilder(key:String):Builder[Set[String]] = this
  def resultType:Class[Set[String]] = classOf[Set[String]]
}

object NameBuilder extends Builder[Name] {
  def init:Name = Name("", "", "")
  def apply(folding:Name, key:String, value:Any) = key match {
    case "given" => folding.copy(given = value.toString)
    case "middle" => folding.copy(middle = value.toString)
    case "family" => folding.copy(family = value.toString)
    case _ => throw new ParseException("Unexpected key: " + key, -1)
  }
  def childBuilder(key:String):Builder[_] = SetBuilder
  override val resultType:Class[Name] = classOf[Name]
}

object PersonBuilder extends Builder[Person] {
  def init:Person = Person(Name("", "", ""), "", false, Set.empty)
  def apply(folding:Person, key:String, value:Any) = key match {
    case "name" => folding.copy(n = value.asInstanceOf[Name])
    case "gender" => folding.copy(gender = value.toString)
    case "isDead" => folding.copy(isDead = (value == true))
    case "interests" => folding.copy(interests = value.asInstanceOf[Set[String]])
    case _ => throw new ParseException("Unexpected key: " + key, -1)
  }
  def childBuilder(key:String):Builder[_] = key match {
    case "name" => NameBuilder
    case _ => SetBuilder
  }
  override val resultType:Class[Person] = classOf[Person]
}

// parse the json file
val p:Person = new JsonParser(PersonBuilder).parse(json)

// at this point, do something with p
System.out.println(p)
