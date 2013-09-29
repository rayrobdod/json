/**
 * These are classes that either implement
 * {@link com.rayrobdod.javaScriptObjectNotation.parser.JSONDecoder} or have
 * a static decode method.
 * </p><p>
 * A possible usage of these would be something like:
 * <pre><code>String s = "{\"Hello\",\"World\"}";
Map&lt;?, ?&gt; o = (Map&lt;?, ?&gt;) {@link com.rayrobdod.javaScriptObjectNotation.parser.decoders.ToJavaCollectionJSONDecoder ToJavaCollectionJSONDecoder}.decode(s);
o.get("Hello"); // returns "World" </code></pre>
 */
package com.rayrobdod.javaScriptObjectNotation.parser.decoders;
