package vf.readaloud.controller.audio

import utopia.annex.util.RequestResultExtensions._
import utopia.echo.model.request.tts.piper.{TextToAudioFileRequest, TtsParams}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Empty
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.parse.string.Regex
import utopia.flow.util.StringExtensions._
import utopia.flow.util.TryCatch
import utopia.flow.util.TryExtensions._
import vf.readaloud.model.text.SpokenText
import vf.readaloud.model.text.pdf.{PdfPage, SpokenPdfPage, SpokenPdfSection}
import vf.readaloud.util.Common._

import java.nio.file.Path

/**
 * Adds audio to read PDF documents
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
// TODO: Don't make headers so noisy
object GenerateAudio
{
	// ATTRIBUTES   -------------------
	
	private val fileNameUpdateRegex = (!Regex.letterOrDigit).oneOrMoreTimes
	
	
	// OTHER    -----------------------
	
	/**
	 * Generates a spoken version of a PDF document
	 * @param pdf The document to convert
	 * @param directory Directory where the generated audio files will be placed
	 * @param fileNamePrefix Prefix added to all file names (optional)
	 * @return Document pages with audio included. May yield full or partial failures.
	 */
	def to(pdf: Seq[PdfPage], directory: Path, fileNamePrefix: => String = "")
	      (implicit settings: TtsParams) =
	{
		if (pdf.isEmpty)
			TryCatch.Success(Empty)
		else
			directory.createDirectories().flatMapCatching { _to(pdf, _, fileNamePrefix, settings) }
	}
	
	// Assumes that directory exists
	private def _to(pdf: Seq[PdfPage], directory: Path, fileNamePrefix: String,
	                settings: TtsParams) =
	{
		// Uses slightly different settings for headers
		lazy val headerSettings = settings.slower
		pdf.iterator.zipWithIndex
			.map { case (page, pageIndex) =>
				println(s"Processing page ${ pageIndex + 1 }/${ pdf.size }")
				page.sections.iterator.zipWithIndex
					.map { case (section, sectionIndex) =>
						val pathPrefix = s"${ fileNamePrefix.appendIfNotEmpty("-") }${ pageIndex + 1 }-${
							sectionIndex + 1 }${
							section.header.take(14).untilLast(" ").replaceEachMatchOf(fileNameUpdateRegex, "-")
								.prependIfNotEmpty("-") }"
						
						// Converts the header and each paragraph to a separate audio file
						(section.header.ifNotEmpty.map { (_, 0, true) } ++
							section.paragraphs.zipWithIndex.map { case (paragraph, index) =>
								(paragraph, index + 1, false)
							})
							.map { case (text, contentIndex, isHeader) =>
								implicit val appliedSettings: TtsParams = if (isHeader) headerSettings else settings
								lazy val path = directory/s"$pathPrefix-$contentIndex.wav"
								piper.push(TextToAudioFileRequest(text, path)).future.waitForResult().toTry
									.map { SpokenText(text, _) -> isHeader }
							}
							.toTryCatch.map { content =>
								val (paragraphs, header) = content.divideWith { case (content, isHeader) =>
									if (isHeader) Right(content) else Left(content)
								}
								SpokenPdfSection(header.headOption, paragraphs)
							}
					}
					.toTryCatch.map { SpokenPdfPage(_, page.pageHeader) }
			}
			.toTryCatch
	}
}
