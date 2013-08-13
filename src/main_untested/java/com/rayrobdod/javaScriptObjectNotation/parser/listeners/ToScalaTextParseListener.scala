package com.rayrobdod.javaScriptObjectNotation.parser.listeners;

import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener;
import scala.text.{Document, DocNil, DocText, DocNest, DocBreak}

/**
 * A JSONParseListener that converts JSON text into a {@link scala.text.Document}
 * 
 * @author Raymond Dodge
 * @version 02 Jul 2011
 * @version 15 Dec 2011 - moved from {@code net.verizon.rayrobdod.javaScriptObjectNotation} to {@code com.rayrobdod.javaScriptObjectNotation;}
 * @version 15 Jan 2011 - moved from {@code com.rayrobdod.javaScriptObjectNotation.parser}
		to {@code com.rayrobdod.javaScriptObjectNotation.parser.listeners}
 */
class ToScalaTextParseListener extends JSONParseListener
{
	var document:Document = DocNil
	var level:Int = 0
	val spacesPerLevel = 2
	
	override def charRead(index:Int, c:Char) {
		document = document :: DocText(c.toString)
	}
	
	override def elemStarted(index:Int, c:Char) {
		document = document :: DocNest(spacesPerLevel * level, DocText(c.toString)) :: DocBreak
	}
	
	override def elemEnded(index:Int, c:Char) {
		document = document :/: DocNest(spacesPerLevel * level, DocText(c.toString))
	}
	
	override def keyValueSeparation(index:Int, c:Char) {
		document = document :: DocNest(spacesPerLevel * level, DocText(" " + c.toString + " "))
	}
	
	override def openingBracket(index:Int, c:Char) {
//		document = document :: DocNest(spacesPerLevel * level, DocText(c.toString))
		level = level + 1
	}
	
	override def endingBracket(index:Int, c:Char) {
		level = level - 1
//		document = document :/: DocNest(spacesPerLevel * level, DocText(c.toString))
	}
	
	override def abort() = false
	override def ended() {}
	override def started() {document = DocNil; level = 0}
}
