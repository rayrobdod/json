## Goals

* The library shall be capable of parsing of multiple JSON-family formats
  * JSON  http://json.org/
  * CBOR  https://tools.ietf.org/html/rfc7049
  * BSON  http://bsonspec.org/spec.html
  * CSV   https://tools.ietf.org/html/rfc4180
* The parsers should all have the same interface
* The parsers should have a streaming interface
* The parsers shall use a Strategy Pattern to determine how to deserialize an object

## Non-Goals

* Preservation of whitespace, comments or order during a round-trip 
