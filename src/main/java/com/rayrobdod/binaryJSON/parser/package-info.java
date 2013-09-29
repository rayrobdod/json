/**
	<p>These are the specific operations related to processing BSON files.</p>
	<p>
		The main class of these are the
		{@link com.rayrobdod.binaryJSON.parser.BSONParser BSONParser} class and the
		{@link com.rayrobdod.binaryJSON.parser.BSONParseListener BSONParseListener}.
		All the other classes in this package implement BSONParseListener.
		Standard usagage consists simply of creating a BSONParser, then running any BSONParseListener through
		one of the parse methods. The results can then be obtained form the BSONParseListener.
	</p>
	<pre><code>// initiate an instance of a BSONParseListener
{@link com.rayrobdod.binaryJSON.parser.listeners.GetSize} l = new GetSize();
// run <a href="BSONParser.html#parse(com.rayrobdod.binaryJSON.parser.BSONParseListener, java.io.DataInput)"
		>BSONParser#parse(JSONParseListener, Reader)</a> using the just-made BSONParseListener and a previously created reader.
BSONParser.parse(l, reader);
// obtain results from the BSONParseListener
int size = l.getCount();</code></ore>
*/
package com.rayrobdod.binaryJSON.parser;