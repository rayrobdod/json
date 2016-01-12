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
