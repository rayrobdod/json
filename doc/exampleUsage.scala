#!scala

import com.rayrobdod.json.parser.JsonParser;
import com.rayrobdod.json.builder.Builder;

case class Name(given:String, middle:String, family:String)
case class Person(n:Name, gender:String, isDead:boolean, interests:Set[String])

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

object SetBuilder extends Builder[Set[String]] {
	def init:Set[String] = Set.empty
	def apply(folding:Set[String], key:String, value:Object) = {
		folding + value.toString
	}
	def childBuilder(key:String):Builder[Set[String]] = this
}

object NameBuilder extends Builder[Name] {
	def init:Name = Name("", "", "")
	def apply(folding:Name, key:String, value:Object) = key match {
		case "given" => folding.copy(given = value)
		case "middle" => folding.copy(middle = value)
		case "family" => folding.copy(family = value)
		case _ => throw new ParseException("Unexpected key: " + key, -1)
	}
	def childBuilder(key:String):Builder[_] = SetBuilder
	override val resultType:Class[Name] = classOf[Name]
}

object PersonBuilder extends Builder[Person] {
	def init:Person = Person(Name("", "", ""), "", false, Set.empty)
	def apply(folding:Person, key:String, value:Object) = key match {
		case "n" => folding.copy(n = value.asInstanceOf[Name])
		case "gender" => folding.copy(gender = value.toString)
		case "isDead" => folding.copy(isDead = (value == true))
		case "interests" => folding.copy(interests = value.asInstanceOf[Set[String]])
		case _ => throw new ParseException("Unexpected key: " + key, -1)
	}
	def childBuilder(key:String):Builder[_] = key match {
		case "n" => NameBuilder,
		case _ => SetBuilder
	}
	override val resultType:Class[Person] = classOf[Person]
}

val p:Person = JSONParser(PersonBuilder).parse(json)
