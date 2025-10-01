package vf.readaloud.test

import utopia.flow.parse.file.FileExtensions._
import vf.readaloud.controller.pdf.ReadPdf

/**
 * Tests PDF to text -conversion
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
object PdfToTextTest extends App
{
	val text = ReadPdf("data/test-data/articles/Suomalaista-sosiaalipedagogiikkaa-rakentamassa.pdf").get.mkString("\n\n--------------\n\n")
	println()
	println(text)
}
