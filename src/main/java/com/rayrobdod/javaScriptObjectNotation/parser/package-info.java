/**
	<p>These are the specific operations related to processing JSON files.</p>
	<p>
		The main class of these are the
		{@link com.rayrobdod.javaScriptObjectNotation.parser.JSONParser JSONParser} class and the
		{@link com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener JSONParseListener}. 
		All the other classes in this package implement JSONParseListener.
		Standard usagage consists simply of creating a JSONParser, then running any JSONParseListener through
		one of the parse methods. The results can then be obtained form the JSONParseListener.
	</p>
	<pre><code>// initiate an instance of a JSONParseListener
{@link com.rayrobdod.javaScriptObjectNotation.parser.listeners.GetSize} l = new GetSize();
// run <a href="JSONParser.html#parse(com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener, java.io.Reader)"
		>JSONParser#parse(JSONParseListener, Reader)</a> using the just-made JSONParseListener and a previously created reader.
JSONParser.parse(l, reader);
// obtain results from the JSONParseListener
int size = l.getCount();</code></pre>
*/
package com.rayrobdod.javaScriptObjectNotation.parser;