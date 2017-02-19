## 4.0
* Represent failures within the type system
  * Add a type parameter to Parser to represent ways that the Parser can fail
  * Add a type parameter to Builder to represent ways that the Builder can fail
  * Replace `Failure` with `ParserFailure` and `BuilderFailure`
  * Add two type parameters to ParserRetVal (one for ParserFailure and one for BuilderFailure)
  * Make methods that used to return `Either[(String,Int),A]` return `ParserRetVal[A,Nothing,PF,BF]`
* Make easier to use `ParserRetVal` when one of the "success" values is `Nothing`


## 3.1
* Remove deprecated builders and parsers
  - BeanBuilder, CaseClassBuilder, CborObjectBuilder, CborArrayBuilder,
    MinifiedJsonOjectBuilder, MinifiedJsonArrayBuilder, CaseClassParser
* Add Builder and Parser transforms
  - flatMapKey
  - zip (on Builder)
* Add ComplexProjection::flatMap

* Change JsonValue.Number from containing a "Number" to containing a "BigDecimal"
* Change CborValue.Number from containing a "Number" to containing a "Rational"
  * Deprecate JsonValue::cborValueHexencodeByteStr
  * Add JsonValue::cborValue2JsonValueEither

* Add number tags and HalfFloat support to CborParser
* Add number tags support to CborBuilder

* Rename MapBuilder::apply2 to another overload of MapBuilder::apply
* Add RecursiveMapParser
* Change JsonParser and CsvParser's input type for better performance
* Increase strictness of number parsing in JsonParser
* Catch stack overflows inside a JsonParser
* CborBuilder reads int32 values larger than Integer.MaxValue (2 ** 31 - 1) correctly
  - int64 values larger than Long.MaxValue (2 ** 63 - 1) are still not read correctly
* Add PiecewiseParser::KeyDefSyntax
* Add shorhand factory methods for PrimitiveSeqParser, PrettyJsonBuilder and IdentityParser

## 3.0.1
* Various optimizations
* Fix JsonParer's parsing of nested objects containing strings containing `[]{}` characters

## 3.0
* Give the recursive map parser (the zero param apply method) a not-Any type parameter

## 3.0-RC2
* Isolate CborValueByteStr backing value from inputs or outputs
* Made Builder and Parser co- and contravariant
* Add projections to ParserRetVal cases
* Add xxxToEither methods to CborValue and JsonValue
* Add flatMapValue method to Builder and Parser
* Remove unwrap/anyWrap methods from JsonValue and CborValue
* Deprecate BeanBuilder, CaseClassBuilder and CaseClassParser
* Rename BuildableBuilder to PiecewiseBuilder
* Add PiecewiseParser and CsvWithHeaderParser

## 3.0-RC1
Typesafety
* Give Builders and Parsers type parameters related to types of Keys and Values
* Add Union types
* Add PrettyJsonBuilder; deprecate MinifiedJsonObjectBuilder and MinifiedJsonArrayBuilder
* Add CborBuilder; deprecate CborObjectBuilder and CborArrayBuilder
* Add BuildableBuilder

## 2.0
* None

## 2.0-RC6
* Build scala2.10 and scala2.11 versions using java7

## 2.0-RC5
* Make every class, excepting Builder, final
* Allow CaseClassBuilder's clazz parameter to be provided implicitly
* Allow MapParser and SeqParser to take collections with type params other than Any
* Change BeanBuilder and CaseClassBuilder's childBuilders parameter from a Map to a Function1

## 2.0-RC4
* JsonParser will no longer accept trailing commas
* Creating a set of classes that allow writing to json/cbor format files

## 2.0-RC3
* JsonParser can now parse non-integer numbers

## 2.0-RC2
* improved code style
* removing com.rayrobdod.json.builder.StringBuilder
* allowing whitespace padding after numbers in JSONParser
* allowing U+FEFF padding in beginning of JSONParser
* treating U+FEFF as whitespace in CSV and TSV documents documents

## 2.0-RC1
Recreating wholesale
