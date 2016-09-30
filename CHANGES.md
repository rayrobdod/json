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
