/**
 * These are classes that either implement {@link com.rayrobdod.javaScriptObjectNotation.parser.JSONDecoder} or have a static decode method. 
 * </p><p>
 * A possible usage of these would be something like:
	<code style="white-space:pre;">
String s = "{\"Hello\",\"World\"}";
Map&lt;?, ?&gt; o = (Map&lt;?, ?&gt;) {@link com.rayrobdod.javaScriptObjectNotation.parser.decoders.ToJavaCollectionJSONDecoder ToJavaCollectionJSONDecoder}.decode(s)
o.get("Hello") // returns "World"
	</code>
 */
 package com.rayrobdod.javaScriptObjectNotation.parser.decoders;
