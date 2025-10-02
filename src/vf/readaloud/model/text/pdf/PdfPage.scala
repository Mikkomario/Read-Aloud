package vf.readaloud.model.text.pdf

import utopia.flow.util.StringExtensions._

/**
 * Represents a PDF page where the text layout has already been interpreted
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
case class PdfPage(sections: Seq[PdfSection], pageHeader: String, footer: String)
{
	override def toString = s"${ pageHeader.mapIfNotEmpty{ h => s"[$h]\n\n" } }${ sections.mkString("\n\n") }"
}