package com.rayrobdod.json.benchmark

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit
import java.nio.charset.StandardCharsets.UTF_8

@BenchmarkMode(Array(Mode.Throughput, Mode.SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
//@Fork(value = 3, jvmArgsAppend = Array("-server", "-disablesystemassertions"))
@Fork(value = 3, jvmArgsAppend = Array())
class JsonParserBench {
	import JsonParserBench._
	
	@Benchmark
	@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
	def quix_rrd() = {
		import com.rayrobdod.json.parser.JsonParser
		import com.rayrobdod.json.builder.MapBuilder
		import com.rayrobdod.json.union.{StringOrInt, JsonValue}
		
		val parser = new JsonParser
		val builder = MapBuilder.apply[StringOrInt, JsonValue]
		
		parser.parse(builder, new java.io.InputStreamReader(this.getClass.getClassLoader.getResourceAsStream("qux1.json"), UTF_8))
	}
	
	@Benchmark
	@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
	def quix_jawn() = {
		// I'm looking for a way to turn a Resource into a ReadableByteChannel and not finding one
		val str = resourceToString("qux1.json", 0x60000);
		jawn.ast.JParser.parseFromString(str).get
	}
	
	@Benchmark
	@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
	def model_rrdpeicewise() = {
		import com.rayrobdod.json.parser.JsonParser
		import com.rayrobdod.json.builder._
		import com.rayrobdod.json.builder.PiecewiseBuilder._
		import com.rayrobdod.json.union.{StringOrInt, JsonValue, ParserRetVal}
		val parser = new JsonParser
		val twoIntsToPairBuilder = new PiecewiseBuilder[StringOrInt, JsonValue, (Int, Int)]( ((1,1)) )
				.addDef(0, partitionedPrimitiveKeyDef[StringOrInt, JsonValue, (Int, Int), Int]({case x => x.integerToEither{x => Right(x)}}, (f,m) => f.copy(_1 = m)))
				.addDef(1, partitionedPrimitiveKeyDef[StringOrInt, JsonValue, (Int, Int), Int]({case x => x.integerToEither{x => Right(x)}}, (f,m) => f.copy(_2 = m)))
		val builder = new SeqBuilder(
			new PiecewiseBuilder[StringOrInt, JsonValue, Weapon](new Weapon("MISSING", 0, 0, 0, (1,1)))
				.addDef("name", partitionedPrimitiveKeyDef[StringOrInt, JsonValue, Weapon, String]({case x => x.stringToEither{x => Right(x)}}, (f,m) => f.copy(name = m)))
				.addDef("cost", partitionedPrimitiveKeyDef[StringOrInt, JsonValue, Weapon, Int]({case x => x.integerToEither{x => Right(x)}}, (f,m) => f.copy(cost = m)))
				.addDef("power", partitionedPrimitiveKeyDef[StringOrInt, JsonValue, Weapon, Int]({case x => x.integerToEither{x => Right(x)}}, (f,m) => f.copy(power = m)))
				.addDef("accuracy", partitionedPrimitiveKeyDef[StringOrInt, JsonValue, Weapon, Int]({case x => x.integerToEither{x => Right(x)}}, (f,m) => f.copy(accuracy = m)))
				.addDef("range", partitionedKeyDef[StringOrInt, JsonValue, Weapon, (Int, Int), (Int, Int)](
					twoIntsToPairBuilder,
					{
						case ParserRetVal.Complex(x) => Right(x)
						case ParserRetVal.Primitive(JsonValue.JsonValueNumber(x)) => Right( ((x.intValue, x.intValue)) )
					},
					(f,m) => f.copy(range = m)
				))
				.ignoreUnknownKeys
		)
		
		parser.parse(builder, new java.io.InputStreamReader(this.getClass.getClassLoader.getResourceAsStream("model.json"), UTF_8)).fold(
			{x => x}, {x => x}, {(s,i) => throw new java.text.ParseException(s + " " + i, i)}
		)
	}
	
	@Benchmark
	@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
	def model_rrddirect() = {
		import com.rayrobdod.json.parser.{Parser, JsonParser}
		import com.rayrobdod.json.builder._
		import com.rayrobdod.json.union.{StringOrInt, JsonValue}
		val parser = new JsonParser
		val twoIntsToPairBuilder = new Builder[StringOrInt, JsonValue, (Int, Int)] {
			def init = (1,1)
			override def apply[Input](folding:(Int, Int), key:StringOrInt, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):Either[(String, Int), (Int, Int)] = {
				parser.parsePrimitive(input).right.flatMap{_.integerToEither{value:Int =>
					key match {
						case StringOrInt.Right(0) => Right(folding.copy(_1 = value))
						case StringOrInt.Right(1) => Right(folding.copy(_2 = value))
						case x => Left("Illegal Key: " + x, 0)
					}
				}}
			}
		}
		val builder = new SeqBuilder(
			new Builder[StringOrInt, JsonValue, Weapon] {
				def init = new Weapon("MISSING", 0, 0, 0, (1,1))
				def apply[Input](folding:Weapon, key:StringOrInt, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):Either[(String, Int), Weapon] = {
					key match {
						case StringOrInt.Left("name") => parser.parsePrimitive(input).right.flatMap{_.stringToEither{x => Right(x)}}.right.map{x => folding.copy(name = x)}
						case StringOrInt.Left("cost") => parser.parsePrimitive(input).right.flatMap{_.integerToEither{x => Right(x)}}.right.map{x => folding.copy(cost = x)}
						case StringOrInt.Left("power") => parser.parsePrimitive(input).right.flatMap{_.integerToEither{x => Right(x)}}.right.map{x => folding.copy(power = x)}
						case StringOrInt.Left("accuracy") => parser.parsePrimitive(input).right.flatMap{_.integerToEither{x => Right(x)}}.right.map{x => folding.copy(accuracy = x)}
						case StringOrInt.Left("range") => parser.parse(twoIntsToPairBuilder, input).fold(
							{x => Right(folding.copy(range = x))},
							{x => x.integerToEither{x => Right(x)}.right.map{x => folding.copy(range = ((x,x)))}},
							{(s,i) => Left((s,i))}
						)
						case _ => Right(folding)
					}
				}
			}
		)
		
		parser.parse(builder, new java.io.InputStreamReader(this.getClass.getClassLoader.getResourceAsStream("model.json"), UTF_8)).fold(
			{x => x}, {x => x}, {(s,i) => throw new java.text.ParseException(s + " " + i, i)}
		)
	}
	
	@Benchmark
	@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
	def model_jawn() = {
		val str = resourceToString("model.json", 1024);
		val ast = jawn.ast.JParser.parseFromString(str).get
		
		(0 until 5).map{ast.get _}.map{x =>
			new Weapon(
				x.get("name").asString,
				x.get("cost").asInt,
				x.get("power").asInt,
				x.get("accuracy").asInt,
				{
					val range = x.get("range")
					val isInt = range.getInt.map{i => ((i,i))}
					val isSeq = range.get(0).getInt.zip(range.get(1).getInt).headOption
					isInt.orElse(isSeq).getOrElse( ((1, 1)) )
				}
			)
		}
	}
}

object JsonParserBench {
	final case class Weapon(name:String, cost:Int, power:Int, accuracy:Int, range:(Int, Int))
	
	def resourceToString(resName:String, bufSize:Int):String = {
		val buf = new Array[Char](bufSize);
		val size = new java.io.InputStreamReader(this.getClass.getClassLoader.getResourceAsStream(resName), UTF_8).read(buf)
		new String(buf, 0, size)
	}
}
