package com.rayrobdod.json.benchmark

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit
import java.nio.charset.StandardCharsets.UTF_8

@BenchmarkMode(Array(Mode.Throughput, Mode.SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 3, jvmArgsAppend = Array())
class CsvParserBench {
	
	@Benchmark
	@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
	def rrd() = {
		import com.rayrobdod.json.parser.CsvParser
		import com.rayrobdod.json.builder._
		
		val parser = new CsvParser
		val builder = new SeqBuilder(new PrimitiveSeqBuilder[String])
		
		parser.parse(builder, new java.io.InputStreamReader(this.getClass.getClassLoader.getResourceAsStream("samp.csv"), UTF_8))
	}
}
