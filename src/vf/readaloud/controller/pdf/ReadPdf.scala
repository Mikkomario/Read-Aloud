package vf.readaloud.controller.pdf

import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.text.{PDFTextStripper, TextPosition}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.{Empty, OptimizedIndexedSeq, Pair, Single}
import utopia.flow.parse.AutoClose._
import utopia.flow.parse.string.Regex
import utopia.flow.view.mutable.Pointer
import utopia.paradigm.measurement.DistanceExtensions._
import utopia.paradigm.measurement.DistanceUnit.Dtp
import utopia.paradigm.shape.shape2d.vector.point.Point
import vf.readaloud.model.text.pdf.{PdfPage, PdfSection, PdfTextEntry}

import java.nio.file.Path
import java.util
import scala.collection.immutable.VectorBuilder

/**
 * Used for converting PDF files into text
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
// TODO: Handle page footers
object ReadPdf
{
	// ATTRIBUTES   -----------------------
	
	private val maxPageHeaderY = 2.cm.toUnit(Dtp)
	private val minPageHeight = 4.cm.toUnit(Dtp)
	private val insignificantXDiff = 3
	
	/**
	 * A regular expression that matches lines which have been spit mid-word
	 */
	private val midWordSplitLineRegex = Regex.any + Regex.letter + Regex.escape('-') + Regex.endOfString
	
	
	// OTHER    ---------------------------

	def apply(path: Path) = {
		PDDocument.load(path.toFile).tryConsume { doc =>
			// Creates the extractor
			val extractor = new Extractor()
			extractor.setSortByPosition(true)

			// Processes the book text
			extractor.getText(doc)
			extractor.result()
		}
	}


	// NESTED   ---------------------------

	private class Extractor extends PDFTextStripper
	{
		// ATTRIBUTES   ------------------------

		private val pagesBuilder = new VectorBuilder[PdfPage]()
		private val pageBuilder = new VectorBuilder[PdfTextEntry]()
		private val lastYP = Pointer(0.0)


		// IMPLEMENTED  ------------------------

		override def writeString(text: String, textPositions: util.List[TextPosition]) = {
			val pos = textPositions.get(0)
			// Tracks text position to recognize line changes
			val y = pos.getY.toDouble
			// Case: New page starts
			if (y - lastYP.getAndSet(y) < -minPageHeight)
				buildPage()
			
			// Case: Page continues
			pageBuilder += PdfTextEntry(text, Point(pos.getX.toDouble, y), pos.getFont.getName)
		}


		// OTHER    ----------------------------

		def result() = pagesBuilder.result()
		
		private def buildPage(): Unit = {
			val textEntries = pageBuilder.result()
			if (textEntries.nonEmpty) {
				pageBuilder.clear()
				
				// Determines the normal text properties (start X, font, line Y progress)
				val standardStartX = textEntries.iterator.map { _.x }.mostCommonEntry
				val standardFont = textEntries.iterator.map { _.font }.mostCommonEntry
				val standardLineHeight = textEntries.iterator
					.paired.map { _.mapAndMerge { _.y } { (prev, next) => next - prev } }
					.filter { _ > 0 }.mostCommonEntryOption.getOrElse(0.0)
				
				// Looks for the page header
				// The header must have a low Y value, and fulfill at least one of the following:
				//      1. Has different font
				//      2. Has different X coordinate
				//      3. Has a large gap until the next line
				val potentialPageHeaderEntries = textEntries.takeWhile { _.y <= maxPageHeaderY }
				val pageHeaderEntries = {
					if (potentialPageHeaderEntries.isEmpty)
						Empty
					else {
						val minHeaderGap = standardLineHeight * 3
						potentialPageHeaderEntries.zipWithIndex.reverseIterator.find { case (entry, i) =>
							textEntries.lift(i + 1).exists { nextEntry => nextEntry.y - entry.y >= minHeaderGap }
						} match {
							case Some((_, gapIndex)) => potentialPageHeaderEntries.take(gapIndex + 1)
							case None =>
								potentialPageHeaderEntries
									.takeWhile { e => e.x != standardStartX || e.font != standardFont }
						}
					}
				}
				
				// Splits the remaining text into sections based on header lines.
				// Header lines are those that have a different font
				val bodyEntries = textEntries.drop(pageHeaderEntries.size)
				val headerIndices = bodyEntries.iterator.zipWithIndex
					.filter { case (entry, _) => entry.font != standardFont }.map { _._2 }.toIntSet
				val (sections, footer) = {
					val yDiffThreshold = standardLineHeight * 1.25
					val xThreshold = standardStartX + insignificantXDiff
					
					if (headerIndices.isEmpty)
						Single(sectionFrom("", bodyEntries, yDiffThreshold, xThreshold)) -> ""
					else {
						val sectionsBuilder = OptimizedIndexedSeq.newBuilder[PdfSection]
						// Adds the first section
						val firstHeaderIndex = headerIndices.head
						if (firstHeaderIndex > 0)
							sectionsBuilder += sectionFrom("", bodyEntries.take(firstHeaderIndex),
								yDiffThreshold, xThreshold)
						// Adds the middle section
						headerIndices.ranges.iterator.paired.foreach { headerRanges =>
							val header = entriesToText(bodyEntries.slice(headerRanges.first))
							val body = bodyEntries.slice(headerRanges.first.end + 1, headerRanges.second.start)
							sectionsBuilder += sectionFrom(header, body, yDiffThreshold, xThreshold)
						}
						// Adds the last section
						// However, if the last section only consists of a header (i.e. different font),
						// and has a different X-positioning throughout, it will be considered a footer instead
						val lastHeaderEntries = bodyEntries.slice(headerIndices.ranges.last)
						val lastHeaderText = entriesToText(lastHeaderEntries)
						val footer = {
							if (headerIndices.last == bodyEntries.size - 1 &&
								lastHeaderEntries.forall { e => (e.x - standardStartX).abs > insignificantXDiff })
								lastHeaderText
							else {
								sectionsBuilder += sectionFrom(lastHeaderText, bodyEntries.drop(headerIndices.last + 1),
									yDiffThreshold, xThreshold)
								""
							}
						}
						
						sectionsBuilder.result() -> footer
					}
				}
				
				pagesBuilder += PdfPage(sections, entriesToText(pageHeaderEntries), footer)
			}
		}
		
		private def sectionFrom(header: String, entries: Seq[PdfTextEntry], yDiffThreshold: Double, xThreshold: Double) = {
			// Divides the content into paragraphs, based on longer line separators and non-standard X values
			val longerLineSeparatorIndices = entries.iterator.paired.zipWithIndex
				.filter { case (entries, _) =>
					entries.mapAndMerge { _.y } { (prev, next) => (next - prev) >= yDiffThreshold }
				}
				.map { _._2 }.toIntSet
			val differentXIndices = entries.iterator.zipWithIndex.filter { _._1.x >= xThreshold }.map { _._2 }.toIntSet
			val paragraphStartIndices = (longerLineSeparatorIndices ++ differentXIndices).ranges
				.flatMap { range =>
					range.only match {
						case Some(onlyIndex) => Single(onlyIndex)
						case None => Pair(range.start, range.end + 1)
					}
				}
				.startingWith(0).endingWith(entries.size)
			
			// Builds the paragraphs
			val paragraphs = paragraphStartIndices.paired.map { ends =>
				entriesToText(entries.slice(ends.first, ends.second))
			}
			PdfSection(header, paragraphs)
		}
		
		private def entriesToText(entries: IterableOnce[PdfTextEntry]) = {
			val builder = new StringBuilder()
			var lastEntry = ""
			entries.iterator.foreach { entry =>
				// Adds a separator between the entries, if appropriate
				if (lastEntry.nonEmpty && lastEntry.last != ' ' && !entry.text.startsWith(" "))
					builder += ' '
				// Handles the situation where appending a line split mid-word
				val appliedText = {
					if (midWordSplitLineRegex(entry.text)) {
						lastEntry = ""
						entry.text.dropRight(1)
					}
					else {
						lastEntry = entry.text
						entry.text
					}
				}
				builder ++= appliedText
			}
			builder.result().trim
		}
	}
}
