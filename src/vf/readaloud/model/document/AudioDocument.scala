package vf.readaloud.model.document

import utopia.flow.collection.CollectionExtensions._
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.factory.FromModelFactoryWithSchema
import utopia.flow.generic.model.immutable.{Model, ModelDeclaration}
import utopia.flow.generic.model.mutable.DataType.StringType
import utopia.flow.generic.model.template.ModelConvertible
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.parse.file.FileUtils
import utopia.flow.time.Now
import utopia.flow.util.TryExtensions._
import vf.readaloud.model.document.pdf.SpokenPdfPage
import vf.readaloud.util.Common._

import java.io.FileNotFoundException
import java.nio.file.Path
import java.time.Instant
import java.util.UUID
import scala.util.{Failure, Success}

object AudioDocument extends FromModelFactoryWithSchema[AudioDocument]
{
	// ATTRIBUTES   -------------------------
	
	override lazy val schema: ModelDeclaration = ModelDeclaration(
		"id" -> StringType, "name" -> StringType, "directory" -> StringType, "pdf" -> StringType)
	
	
	// IMPLEMENTED  -------------------------
	
	override protected def fromValidatedModel(model: Model): AudioDocument =
		apply(model("id").getString, model("name").getString, model("directory").getString, model("pdf").getString,
			model("audioPath").getString, model("conversion").int, model("created").getInstant)
			
	
	// OTHER    ----------------------------
	
	/**
	 * Creates a new document, collecting the associated files
	 * @param name Name of this document
	 * @param pdf The PDF document used when creating this document
	 * @return A new document. Failure if failed to move or create files.
	 */
	def newDocument(name: String, pdf: Path) = NewAudioDocument(name, pdf)
	
	
	// NESTED   --------------------------
	
	case class NewAudioDocument(name: String, pdf: Path)
	{
		// ATTRIBUTES   ------------------
		
		/**
		 * Directory where the files in this document will be placed
		 */
		lazy val directory = (dataDirectory/s"documents/${ FileUtils.normalizeFileName(name) }").unique
		/**
		 * Directory where the audio files for this document should be stored
		 */
		lazy val audioDirectory = directory/"audio"
		
		
		// OTHER    ---------------------
		
		/**
		 * Converts this prepared document into an actual audio document
		 * @param pages Pages to include in this document
		 * @param paused Whether audio-generation was paused (default = false)
		 * @return Generated audio document. Failure if file-interaction failed at some level.
		 */
		def initialize(pages: Seq[SpokenPdfPage], paused: Boolean = false) = {
			directory.createDirectories().flatMap { directory =>
				// Copies the PDF
				pdf.copyTo(directory).flatMap { pdfPath =>
					// Determines the audio directory
					val relativeAudioDirectory = pages.findMap { _.sections.findMap { _.paragraphs.headOption } } match {
						case Some(text) =>
							text.audioPath.parent.relativeTo(directory).toOption
								.toTry { new IllegalArgumentException(
									"The specified audio paths don't reside in this document's directory") }
						case None => Success("audio": Path)
					}
					relativeAudioDirectory.flatMap { relativeAudioDirectory =>
						// Writes the structure JSON documents
						(directory/"structure").createDirectories().flatMap { pagesDir =>
							pages.iterator.zipWithIndex
								.map { case (page, index) =>
									(pagesDir/s"page-${ index + 1 }.json").write(page.toContextualModel.toJson)
								}
								.toTry
								.map { _ =>
									new AudioDocument(UUID.randomUUID().toString, name, directory, pdfPath.fileName,
										relativeAudioDirectory, if (paused) Some(pages.size) else None)
								}
						}
					}
				}
			}
		}
	}
}

/**
 * Represents a (pdf) document that has been converted to audio
 * @param id A unique ID of this document
 * @param name Name given to this document
 * @param directory Directory where this document's files are stored
 * @param pdfFileName Name of the original PDF file. Expected to be stored in 'directory'.
 * @param relativeAudioDirectory Directory where the audio files are stored, relative to 'directory'.
 * @param conversionPausedAtPageIndex Index of the next page to convert to audio, if conversion is still pending.
 *                                    None if conversion has finished.
 * @param created Time when this document was created
 * @author Mikko Hilpinen
 * @since 27.10.2025, v1.0
 */
case class AudioDocument(id: String, name: String, directory: Path, pdfFileName: String,
                         relativeAudioDirectory: Path = "audio",
                         conversionPausedAtPageIndex: Option[Int] = None, created: Instant = Now)
	extends ModelConvertible
{
	// ATTRIBUTES   ------------------------
	
	/**
	 * Directory where the audio files of this document are stored
	 */
	lazy val audioDirectory = directory/relativeAudioDirectory
	
	/**
	 * The pages that form this document. May contain failure(s).
	 */
	lazy val pages = {
		lazy val pageFactory = SpokenPdfPage.withAudioDirectory(audioDirectory)
		(directory/"structure")
			.iterateChildren {
				_.filter { _.fileType == "json" }.toVector.sortBy { _.fileName }
					.map { pageFactory.fromPath(_) }.toTryCatch
			}
			.flattenCatching
	}
	
	
	// IMPLEMENTED  -------------------------
	
	override def toModel: Model = Model.from("id" -> id, "name" -> name, "directory" -> directory.toJson,
		"pdf" -> pdfFileName, "audioPath" -> relativeAudioDirectory.toJson,
		"conversion" -> conversionPausedAtPageIndex, "created" -> created)
	
	
	// OTHER    ----------------------------
	
	/**
	 * Opens the PDF document associated with this document
	 * @return Success or a failure
	 */
	def openPdf() = {
		val path = directory/pdfFileName
		if (path.notExists)
			Failure(new FileNotFoundException(s"$path doesn't exist"))
		else
			path.openInDesktop()
	}
}