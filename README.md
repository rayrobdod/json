# JSON
[![Build Status](https://travis-ci.org/rayrobdod/json.svg?branch=master)](https://travis-ci.org/rayrobdod/json)
[![Coverage Status](https://coveralls.io/repos/rayrobdod/json/badge.svg?branch=master)](https://coveralls.io/r/rayrobdod/json?branch=master)
[![Build status](https://ci.appveyor.com/api/projects/status/091ba0yf3lcxscst/branch/master?svg=true)](https://ci.appveyor.com/project/rayrobdod/json/branch/master)

A library intended to allow easily replacing the serialization of JSON-like data.

## Build Instructions
This repository uses [sbt](http://www.scala-sbt.org/) as its build tool.

The dependencies can be found by sbt automatically, but only scala itself is
used something other than some kind of testing harness.

If you are building the documentation and have an instance of
[graphvis](http://www.graphviz.org/)'s dot executable, you can copy
[dot.sbt.example](dot.sbt.example) to `dot.sbt` and give it a real path to
enable scaladoc's class diagrams. The hierarchy isn't very interesting, but
still.

## Why?

Because, especially in Scala where case classes are easy to write up, but even
otherwise, I have yet to have an JSON, or even XML, abstract syntax tree escape
the scope of a function whose sole purpose is to de/serialize a case class - as
a corollary I've transformed case classes, but never used a zipper. The setup
provided by this library removes the AST step of serialization or
deserialization.

Additionally, this forces a separation of de/serialization logic from the rest
of the program logic. A bit of an enforced discipline thing.

## Usage

The two important base traits in this library are
[Builder](src/main/scala/com/rayrobdod/json/builder/Builder.scala) and [Parser](src/main/scala/com/rayrobdod/json/parser/Parser.scala).
To convert a thing into another thing - for example to deserialize a json format
string into a model object - one implementation of each trait is obtained (which
implementation depends on the input and output), the input and builder implementation
are fed into the Parser's parse method and
that method returns the resulting object (or an error).


An example use is indicated in [/doc/parsingExample.scala](doc/parsingExample.scala)
and [/doc/serializeExample.scala](doc/serializeExample.scala).

