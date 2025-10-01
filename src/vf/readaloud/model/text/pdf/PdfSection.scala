package vf.readaloud.model.text.pdf

import utopia.flow.util.StringExtensions._

/**
 * Represents a section of text found from a PDF document
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
case class PdfSection(header: String, paragraphs: Seq[String])
{
	override def toString = s"${ header.mapIfNotEmpty { h => s"# $h\n" } }${ paragraphs.mkString("\n\n") }"
}