package vf.readaloud.model.text.pdf

import utopia.flow.generic.model.immutable.Model
import utopia.flow.generic.model.template.{ModelConvertible, ModelLike, Property}
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.factory.FromModelFactory
import utopia.flow.util.TryExtensions._
import vf.readaloud.util.Common._

import scala.util.Try

object SpokenPdfPage extends FromModelFactory[SpokenPdfPage]
{
	override def apply(model: ModelLike[Property]): Try[SpokenPdfPage] = model("sections").tryVector
		.flatMap { sectionValues =>
			sectionValues.view.map { _.tryModel.flatMap(SpokenPdfSection.apply) }.toTryCatch
				.logToTryWithMessage("Failed to parse some of the sections")
		}
		.filter { _.nonEmpty }
		.map { sections => apply(sections, model("pageHeader").getString, model("footer").getString) }
}

/**
 * Represents a PDF page which has been converted to spoken audio
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
// WET WET from PdfPage
case class SpokenPdfPage(sections: Seq[SpokenPdfSection], pageHeader: String, footer: String) extends ModelConvertible
{
	override def toModel: Model = Model.from("sections" -> sections, "pageHeader" -> pageHeader, "footer" -> footer)
}
