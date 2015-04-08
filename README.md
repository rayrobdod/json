# JSON


## Build Instructions
This repository uses [sbt](http://www.scala-sbt.org/) as its build tool.

Both dependencies can be found by sbt automatically, and both are only used for
testing.

##Usage
There are two types of classes in this library. The first is
[Builder](tree/master/src/main/java/com/rayrobdod/json/builder/Builder.java),
which has to be implemented by a user of the library. The purpose is to receive
values from the parser and add it to data objects.

The other type of class is the [parsers](tree/master/src/main/scala/com/rayrobdod/json/parser/).
There is one per file format. They universally take a builder as a constructor
parameter and have a 'parse' method taking either a DataInput or a Iterable[Char].


An example use is indicated in [/doc/exampleUsage.scala](tree/master/doc/exampleUsage.scala)

