package vf.readaloud.model.text.pdf

import utopia.paradigm.shape.shape2d.vector.point.Point

/**
 * Represents an individual text entry extracted from a PDF.
 * These are combined to form the whole text.
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
case class PdfTextEntry(text: String, position: Point, font: String)
{
	def x = position.x
	def y = position.y
}
