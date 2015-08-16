# JSON
[![Build Status](https://travis-ci.org/rayrobdod/json.svg?branch=master)](https://travis-ci.org/rayrobdod/json)
[![Coverage Status](https://coveralls.io/repos/rayrobdod/json/badge.svg?branch=master)](https://coveralls.io/r/rayrobdod/json?branch=master)


## Build Instructions
This repository uses [sbt](http://www.scala-sbt.org/) as its build tool.

The dependencies can be found by sbt automatically, but only scala itself is
used something other than some kind of testing harness.

If you are building the documentation and have an instance of
[graphvis](http://www.graphviz.org/)'s dot executable, you can copy
[dot.sbt.example](dot.sbt.example) to `dot.sbt` and give it a real path to
emable scaladoc's class diagrams. The heiarchy isn't very interesting, but
still.

## Usage
There are two types of classes in this library. The first is
[Builder](src/main/scala/com/rayrobdod/json/builder/Builder.scala),
which has to be implemented by a user of the library. The purpose is to receive
values from the parser and add it to data objects. 

The other type of class is the [parsers](src/main/scala/com/rayrobdod/json/parser).
Each parser accepts one file format. They universally take a Builder as a constructor
parameter and contain a single method: 'parse' which takes either a DataInput or
a Iterable[Char] and returning an object.


An example use is indicated in [/doc/exampleUsage.scala](doc/exampleUsage.scala).

