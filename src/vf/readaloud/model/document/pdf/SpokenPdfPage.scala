package vf.readaloud.model.document.pdf

import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.factory.FromModelFactory
import utopia.flow.generic.model.immutable.Model
import utopia.flow.generic.model.template.HasPropertiesLike.HasProperties
import utopia.flow.generic.model.template.ModelConvertible
import utopia.flow.util.TryExtensions._
import vf.readaloud.util.Common._

import java.nio.file.Path
import scala.util.Try

object SpokenPdfPage extends FromModelFactory[SpokenPdfPage]
{
	// IMPLEMENTED  --------------------------
	
	override def apply(model: HasProperties): Try[SpokenPdfPage] = parse(model, SpokenPdfSection)
	
	
	// OTHER    ------------------------------
	
	/**
	 * @param audioDirectory Directory where the audio files are stored
	 * @return Interface for parsing PDF pages, assuming the audio is located in the specified directory
	 */
	def withAudioDirectory(audioDirectory: Path) = SpokenPdfPageFactory(audioDirectory)
	
	private def parse(model: HasProperties, sectionParser: FromModelFactory[SpokenPdfSection]): Try[SpokenPdfPage] =
		model("sections").tryVector
			.flatMap { sectionValues =>
				sectionValues.view.map { _.tryModel.flatMap(sectionParser.apply) }.toTryCatch
					.logToTryWithMessage("Failed to parse some of the sections")
			}
			.filter { _.nonEmpty }
			.map { sections => apply(sections, model("pageHeader").getString, model("footer").getString) }
			
	
	// NESTED   ------------------------------
	
	case class SpokenPdfPageFactory(audioDirectory: Path) extends FromModelFactory[SpokenPdfPage]
	{
		// ATTRIBUTES   ----------------------
		
		private lazy val pageParser = SpokenPdfSection.withAudioDirectory(audioDirectory)
		
		
		// IMPLEMENTED  ----------------------
		
		override def apply(model: HasProperties): Try[SpokenPdfPage] = parse(model, pageParser)
	}
}

/**
 * Represents a PDF page which has been converted to spoken audio
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
// WET WET from PdfPage
case class SpokenPdfPage(sections: Seq[SpokenPdfSection], pageHeader: String, footer: String) extends ModelConvertible
{
	// COMPUTED --------------------------------
	
	/**
	 * @return A model based on this page. Assumes that the audio directory reference is stored separately.
	 */
	def toContextualModel = Model.from(
		"sections" -> sections.map { _.toContextualModel }, "pageHeader" -> pageHeader, "footer" -> footer)
	
	
	// IMPLEMENTED  ----------------------------
	
	override def toModel: Model = Model.from("sections" -> sections, "pageHeader" -> pageHeader, "footer" -> footer)
}
