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
emable scaladoc's class diagrams. The heiarchy isn't very interesting, but
still.

## Why?

Because, especially in Scala where case classes are easy to write up, but even
otherwise, I have yet to have an JSON, or even XML, abstract syntax tree escape
the scope of a function whose sole purpose is to de/serialize a case class - as
a corallary I've transformed case classes, but never used a zipper. The setup
provided by this library removes the AST step of serialization or
deserialization.

Additionally, this forces a separation of de/serialization logic from the rest
of the program logic. A bit of an enforced dicipline thing.

## Usage
There are two types of classes in this library. The first is
[Builder](src/main/scala/com/rayrobdod/json/builder/Builder.scala),
which has to be implemented by a user of the library. The purpose is to receive
values from the parser and add it to data objects. If you know about Simple Api
for Xml, think ContentHandlers.

The other type of class is the [parsers](src/main/scala/com/rayrobdod/json/parser).
Each parser accepts one file format. They universally take a Builder as a
constructor parameter and contain a single method: 'parse' which usually takes
either a DataInput or an Iterable[Char] and returns an object.


An example use is indicated in [/doc/exampleUsage.scala](doc/exampleUsage.scala).

