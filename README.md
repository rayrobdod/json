# JSON
[![Build Status](https://travis-ci.org/rayrobdod/json.svg?branch=master)](https://travis-ci.org/rayrobdod/json)
[![Coverage Status](https://coveralls.io/repos/rayrobdod/json/badge.svg?branch=master)](https://coveralls.io/r/rayrobdod/json?branch=master)
[![Build status](https://ci.appveyor.com/api/projects/status/091ba0yf3lcxscst/branch/master?svg=true)](https://ci.appveyor.com/project/rayrobdod/json/branch/master)

A library intended to allow easily replacing the serialization of JSON-like data.

## Why?

Because, especially in Scala where case classes are easy to write up, but even
otherwise, I have yet to have an JSON, or even XML, abstract syntax tree escape
the scope of a function whose sole purpose is to de/serialize a model object - as
a corollary I've transformed model objects, but never used a AST zipper.
With that in mind, this bypasses the AST stage as much as possible.

To facilitate strong type safety,
[JsonValue](src/main/scala/com/rayrobdod/json/union/JsonValue.scala) and
[CborValue](src/main/scala/com/rayrobdod/json/union/CborValue.scala) Sum Types
do exist, but instances of these types last for as short a time as possible, and
unlike the average json ADT, those types do *not* include an "Array" or "Map"
type. Arrays and maps are instead handled via mutual recursion.

With such short-lived data types, swapping Parsers and Builders even ones that
use different data types is extremely simple.

Additionally, this forces the de/serialization logic to be separate from the rest
of the program logic. A bit of an enforced discipline thing.

## Usage

The two important base traits in this library are
[Builder](src/main/scala/com/rayrobdod/json/builder/Builder.scala) and [Parser](src/main/scala/com/rayrobdod/json/parser/Parser.scala).
To convert a thing into another thing - for example to deserialize a json format
string into a model object - one implementation of each trait is obtained (which
implementation depends on the input and output), the input and builder implementation
are fed into the Parser's parse method and
that method returns the resulting object (or an error).

An example of creating a custom Builder and using it with a built-in Parser is
provided in [/doc/parsingExample.scala](doc/parsingExample.scala).

An example of creating a custom Parser and using it with a built-in Builder is
provided in [/doc/serializeExample.scala](doc/serializeExample.scala).

## Build Instructions
This repository uses [sbt](http://www.scala-sbt.org/) as its build tool.

The dependencies can be found by sbt automatically, but only scala itself is
used something other than some kind of testing harness.

If you are building the documentation and have an instance of
[graphvis](http://www.graphviz.org/)'s dot executable, you can copy
[dot.sbt.example](dot.sbt.example) to `dot.sbt` and give it a real path to
enable scaladoc's class diagrams. The hierarchy isn't very interesting, but
still.

