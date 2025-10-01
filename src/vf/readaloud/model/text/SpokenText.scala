package vf.readaloud.model.text

import utopia.flow.generic.model.immutable.{Model, ModelDeclaration}
import utopia.flow.generic.model.template.ModelConvertible
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.factory.FromModelFactoryWithSchema
import utopia.flow.generic.model.mutable.DataType.StringType
import utopia.flow.parse.file.FileExtensions._

import java.nio.file.Path

object SpokenText extends FromModelFactoryWithSchema[SpokenText]
{
	// ATTRIBUTES   ------------------------
	
	override lazy val schema: ModelDeclaration = ModelDeclaration("text" -> StringType, "path" -> StringType)
	
	
	// IMPLEMENTED  -----------------------
	
	override protected def fromValidatedModel(model: Model): SpokenText =
		apply(model("text").getString, model("path").getString)
}

/**
 * Combines text with an audio file containing that text's spoken version
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
case class SpokenText(text: String, audioPath: Path) extends ModelConvertible
{
	override def toModel: Model = Model.from("text" -> text, "path" -> audioPath.toJson)
}
