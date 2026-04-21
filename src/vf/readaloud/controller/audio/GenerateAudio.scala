package vf.readaloud.controller.audio

import utopia.annex.util.RequestResultExtensions._
import utopia.echo.model.request.tts.piper.{TextToAudioFileRequest, TtsParams}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Empty
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.parse.string.Regex
import utopia.flow.time.TimeExtensions._
import utopia.flow.time.TimeUnit.Minute
import utopia.flow.util.StringExtensions._
import utopia.flow.util.ProgressTracker
import utopia.flow.util.result.TryCatch
import utopia.flow.util.result.TryExtensions._
import utopia.flow.view.immutable.eventful.AlwaysFalse
import utopia.flow.view.template.MaybeSet
import vf.readaloud.controller.pdf.ReadPdf
import vf.readaloud.model.document.AudioDocument.NewAudioDocument
import vf.readaloud.model.document.{AudioDocument, SpokenText}
import vf.readaloud.model.document.pdf.{PdfPage, SpokenPdfPage, SpokenPdfSection}
import vf.readaloud.util.Common._

import java.nio.file.Path
import java.time.format.DateTimeFormatter

/**
 * Adds audio to read PDF documents
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
// TODO: Don't make headers so noisy
object GenerateAudio
{
	// ATTRIBUTES   -------------------
	
	private val hhmm = DateTimeFormatter.ofPattern("HH:mm")
	private val fileNameUpdateRegex = (!Regex.letterOrDigit).oneOrMoreTimes
	
	
	// OTHER    -----------------------
	
	/**
	 * Generates audio for a prepared document
	 * @param document The document for which audio is generated
	 * @param stopFlag A flag that may be set to interrupt audio conversion.
	 *                 If set, the conversion will terminate after the page open at that time, finishes.
	 *                 Default = never set.
	 * @param fileNamePrefix Prefix added to all audio file names (optional)
	 * @param settings Settings to use in text-to-speech (implicit)
	 * @return Generated audio document. May contain failure(s).
	 */
	def forDocument(document: NewAudioDocument, stopFlag: MaybeSet = AlwaysFalse, fileNamePrefix: => String = "")
	               (implicit settings: TtsParams) = {
		// Reads the PDF
		ReadPdf(document.pdf).flatMapCatching { pages =>
			// Converts its contents to audio files
			to(pages, document.audioDirectory, stopFlag, fileNamePrefix = fileNamePrefix)
				.flatMap { case (spokenPages, lastConvertedPageIndex, completed) =>
					// Finalizes the document
					document.initialize(spokenPages,
						pausedAfter = if (completed) None else Some(lastConvertedPageIndex + 1)).toTryCatch
				}
		}
	}
	/**
	 * Continues converting a document to audio
	 * @param document Document to convert
	 * @param stopFlag A flag that may be set to interrupt audio conversion.
	 *                 If set, the conversion will terminate after the page open at that time, finishes.
	 *                 Default = never set.
	 * @param settings Settings to use in text-to-speech (implicit)
	 * @return The updated audio document. Failure if conversion failed.
	 */
	def continueDocument(document: AudioDocument, stopFlag: MaybeSet = AlwaysFalse)(implicit settings: TtsParams) = {
		document.conversionPausedAtPageIndex match {
			// Case: Document conversion was paused last time => Continues the conversion
			case Some(firstPageIndex) =>
				// Reads the remaining PDF pages
				ReadPdf(document.pdf, skipPages = firstPageIndex).flatMapCatching { newPages =>
					// Starts converting them to audio
					to(newPages, document.audioDirectory, stopFlag, firstPageIndex + 1)
						.flatMap { case (newPages, lastConvertedIndex, completed) =>
							// Updates the document to include the new audio
							document.append(newPages,
								newPauseIndex = if (completed) None else Some(firstPageIndex + lastConvertedIndex + 1))
								.toTryCatch
						}
				}
			// Case: Already fully converted => Won't do anything
			case None => TryCatch.Success(document)
		}
	}
	
	/**
	 * Generates a spoken version of a PDF document
	 * @param pdf The document to convert
	 * @param directory Directory where the generated audio files will be placed
	 * @param stopFlag A flag that may be set to interrupt audio conversion.
	 *                 If set, the conversion will terminate after the page open at that time, finishes.
	 *                 Default = never set.
	 * @param firstPageIndex Index of the first page in 'pdf'. Used for correctly naming the generated files.
	 *                       Default = 1 = first PDF page.
	 * @param fileNamePrefix Prefix added to all file names (optional)
	 * @return Returns 3 values, or a failure:
	 *              1. Document pages with audio included
	 *              1. Index (0-based) of the last page that was converted to audio.
	 *                 -1 if no pages were converted.
	 *              1. Whether the audio conversion completed
	 */
	def to(pdf: Seq[PdfPage], directory: Path, stopFlag: MaybeSet = AlwaysFalse, firstPageIndex: Int = 1,
	       fileNamePrefix: => String = "")
	      (implicit settings: TtsParams) =
	{
		if (pdf.isEmpty)
			TryCatch.Success((Empty, firstPageIndex - 1, true))
		else
			directory.createDirectories()
				.flatMapCatching { _to(pdf, _, firstPageIndex, fileNamePrefix, settings, stopFlag) }
	}
	
	// Assumes that directory exists
	private def _to(pdf: Seq[PdfPage], directory: Path, firstIndex: Int, fileNamePrefix: String,
	                settings: TtsParams, stopFlag: MaybeSet) =
	{
		// Prepares progress-tracking
		val pageCount = pdf.size
		val finalPageIndex = firstIndex + pageCount - 1
		val progress = ProgressTracker(0) { _.toDouble / pageCount }
		progress.addListener { e =>
			val etaStr = e.projectedCompletion match {
				case Some(completion) =>
					s"; ETA ${completion.toLocalTime.format(hhmm)} (${
						(completion - e.timestamp).roundTo(Minute).description })"
				case None => ""
			}
			println(s"Converted page ${ firstIndex + e.value }/$finalPageIndex$etaStr")
		}
		
		// Uses slightly different settings for headers
		lazy val headerSettings = settings.slower
		// Stops iteration, if stopFlag gets set
		pdf.iterator.takeWhile { _ => stopFlag.isNotSet }.zipWithIndex
			.map { case (page, pageIndex) =>
				val truePageIndex = firstIndex + pageIndex
				val result = page.sections.iterator.zipWithIndex
					.map { case (section, sectionIndex) =>
						val pathPrefix = s"${ fileNamePrefix.appendIfNotEmpty("-") }$truePageIndex-${
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
					.toTryCatch.map { SpokenPdfPage(_, page.pageHeader, page.footer) -> pageIndex }
				
				// Updates the progress
				progress.value = pageIndex + 1
				result
			}
			.toTryCatch
			.map { pages =>
				val (lastConvertedPageIndex, completed) = pages.lastOption match {
					case Some((_, pageIndex)) => pageIndex -> (pageIndex == (pdf.size - 1))
					case None => -1 -> false
				}
				(pages.map { _._1 }, lastConvertedPageIndex, completed)
			}
	}
}
