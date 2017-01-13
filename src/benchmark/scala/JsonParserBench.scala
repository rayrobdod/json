package com.rayrobdod.json.benchmark

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit
import java.nio.charset.StandardCharsets.UTF_8

@BenchmarkMode(Array(Mode.Throughput, Mode.SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
//@Fork(value = 3, jvmArgsAppend = Array("-server", "-disablesystemassertions"))
@Fork(value = 3, jvmArgsAppend = Array())
class JsonParserBench {
	
	//@Benchmark
	@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
	def _baseline() = ""
	
	@Benchmark
	@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
	def rrd() = {
		import com.rayrobdod.json.parser.JsonParser
		import com.rayrobdod.json.builder._
		import com.rayrobdod.json.union._
		
		val parser = new JsonParser
		// val builder = new PrettyJsonBuilder(new PrettyJsonBuilder.IndentPrettyParams("\t", "\n"))
		// val builder = new ScalaJsonAstBuilder
		val builder = MapBuilder.apply[StringOrInt, JsonValue]
		
		// parser.parse(builder, "[[[[[1,2,3]]]]]")
		// parser.parse(builder, "[1,2,3,4,5]")
		parser.parse(builder, new java.io.InputStreamReader(this.getClass.getClassLoader.getResourceAsStream("qux1.json"), UTF_8))
	}
	
	//@Benchmark
	//@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
	//def rrdPrev() = {
	//	import com.rayrobdod.json.parser.PrevJsonParser
	//	import com.rayrobdod.json.builder._
	//	import com.rayrobdod.json.union._
	//	
	//	val parser = new PrevJsonParser
	//	val builder = MapBuilder.apply[StringOrInt, JsonValue]
	//	
	//	// parser.parse(builder, "[[[[[1,2,3]]]]]")
	//	// parser.parse(builder, "[1,2,3,4,5]")
	//	parser.parse(builder, new java.io.InputStreamReader(this.getClass.getClassLoader.getResourceAsStream("qux1.json"), UTF_8))
	//}
	
	// @Benchmark
	@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
	def jawn_jklj() = {
		// jawn.ast.JParser.parseFromString("[[[[[1,2,3]]]]]").get
		jawn.ast.JParser.parseFromString("[1,2,3,4,5]").get
	}
}
