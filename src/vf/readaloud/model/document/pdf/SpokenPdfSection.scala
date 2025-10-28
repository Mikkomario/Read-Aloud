package vf.readaloud.model.document.pdf

import utopia.flow.generic.factory.FromModelFactory
import utopia.flow.generic.model.immutable.Model
import utopia.flow.generic.model.template.HasPropertiesLike.HasProperties
import utopia.flow.generic.model.template.ModelConvertible
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.util.TryExtensions._
import vf.readaloud.model.document.SpokenText
import vf.readaloud.util.Common._

import java.nio.file.Path
import scala.util.Try

object SpokenPdfSection extends FromModelFactory[SpokenPdfSection]
{
	// IMPLEMENTED  -------------------------
	
	override def apply(model: HasProperties): Try[SpokenPdfSection] = parse(model, SpokenText)
			
	
	// OTHER    ------------------------------
	
	/**
	 * @param audioDirectory Directory where the audio files are stored
	 * @return Interface for parsing spoken PDF sections assuming that audio directory
	 */
	def withAudioDirectory(audioDirectory: Path) = SpokenPdfSectionFactory(audioDirectory)
	
	// Paragraph-parsing must at least partially succeed
	private def parse(model: HasProperties, spokenTextParser: FromModelFactory[SpokenText]) =
		model("paragraphs").tryVector
			.flatMap { paragraphValues =>
				paragraphValues.view.map { _.tryModel.flatMap(spokenTextParser.apply) }
					.toTryCatch.logToTryWithMessage("Failed to parse some of the paragraphs in this section")
			}
			.filter { _.nonEmpty }
			.map { paragraphs =>
				val header = model("header").model
					.flatMap { spokenTextParser(_).logWithMessage("Failed to parse the section header") }
				
				apply(header, paragraphs)
			}
			
	
	// NESTED   ---------------------------
	
	case class SpokenPdfSectionFactory(audioDirectory: Path) extends FromModelFactory[SpokenPdfSection]
	{
		// ATTRIBUTES   -------------------
		
		private lazy val spokenTextFactory = SpokenText.withAudioDirectory(audioDirectory)
		
		
		// IMPLEMENTED  -------------------
		
		override def apply(model: HasProperties): Try[SpokenPdfSection] = parse(model, spokenTextFactory)
	}
}

/**
 * Wraps a [[PdfSection]], including audio
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
// WET WET (from PdfSection)
case class SpokenPdfSection(header: Option[SpokenText], paragraphs: Seq[SpokenText]) extends ModelConvertible
{
	// COMPUTED ----------------------------
	
	/**
	 * @return A model based on this PDF section, assuming that the audio directory is stored separately
	 */
	def toContextualModel = Model.from(
		"header" -> header.map { _.toContextualModel }, "paragraphs" -> paragraphs.map { _.toContextualModel })
	
	
	// IMPLEMENTED  ------------------------
	
	override def toModel: Model = Model.from("header" -> header, "paragraphs" -> paragraphs)
}
