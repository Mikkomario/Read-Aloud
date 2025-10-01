package vf.readaloud.model.text.pdf

import utopia.flow.generic.model.immutable.Model
import utopia.flow.generic.model.template.{ModelConvertible, ModelLike, Property}
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.factory.FromModelFactory
import utopia.flow.util.TryExtensions._
import vf.readaloud.model.text.SpokenText
import vf.readaloud.util.Common._

import scala.util.Try

object SpokenPdfSection extends FromModelFactory[SpokenPdfSection]
{
	// IMPLEMENTED  -------------------------
	
	// Paragraph-parsing must at least partially succeed
	override def apply(model: ModelLike[Property]): Try[SpokenPdfSection] =
		model("paragraphs").tryVector
			.flatMap { paragraphValues =>
				paragraphValues.view.map { _.tryModel.flatMap(SpokenText.apply) }
					.toTryCatch.logToTryWithMessage("Failed to parse some of the paragraphs in this section")
			}
			.filter { _.nonEmpty }
			.map { paragraphs =>
				val header = model("header").model
					.flatMap { SpokenText(_).logWithMessage("Failed to parse the section header") }
				
				apply(header, paragraphs)
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
	override def toModel: Model = Model.from("header" -> header, "paragraphs" -> paragraphs)
}
