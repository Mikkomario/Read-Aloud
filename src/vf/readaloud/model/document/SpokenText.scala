package vf.readaloud.model.document

import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.factory.{FromModelFactory, FromModelFactoryWithSchema}
import utopia.flow.generic.model.immutable.{Model, ModelDeclaration, PropertyDeclaration}
import utopia.flow.generic.model.mutable.DataType.StringType
import utopia.flow.generic.model.template.HasPropertiesLike.HasProperties
import utopia.flow.generic.model.template.ModelConvertible
import utopia.flow.parse.file.FileExtensions._

import java.nio.file.Path
import scala.util.{Failure, Success, Try}

object SpokenText extends FromModelFactoryWithSchema[SpokenText]
{
	// ATTRIBUTES   ------------------------
	
	private lazy val contextualSchema: ModelDeclaration = ModelDeclaration("text" -> StringType)
	override lazy val schema: ModelDeclaration = contextualSchema + PropertyDeclaration("path", StringType)
	
	
	// IMPLEMENTED  -----------------------
	
	override protected def fromValidatedModel(model: Model): SpokenText =
		apply(model("text").getString, model("path").getString)
		
	
	// OTHER    ---------------------------
	
	/**
	 * @param audioDirectory Directory that contains the audio files
	 * @return A factory for constructing spoken text instances in that context
	 */
	def withAudioDirectory(audioDirectory: Path) = SpokenTextFactory(audioDirectory)
		
	
	// NESTED   ---------------------------
	
	case class SpokenTextFactory(audioDirectory: Path) extends FromModelFactory[SpokenText]
	{
		// IMPLEMENTED  -------------------
		
		override def apply(model: HasProperties): Try[SpokenText] = contextualSchema.validate(model).flatMap { model =>
			lazy val text = model("text").getString
			model("fileName").tryString match {
				case Success(fileName) => Success(new SpokenText(text, audioDirectory/fileName))
				case Failure(error) =>
					model("path").string match {
						case Some(path) => Success(new SpokenText(text, path))
						case None => Failure(error)
					}
			}
		}
	}
}

/**
 * Combines text with an audio file containing that text's spoken version
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
case class SpokenText(text: String, audioPath: Path) extends ModelConvertible
{
	// COMPUTED -----------------------------
	
	/**
	 * @return A model containing the properties of this spoken text.
	 *         Assumes that the audio directory path is stored elsewhere.
	 */
	def toContextualModel = Model.from("text" -> text, "fileName" -> audioPath.fileName)
	
	
	// IMPLEMENTED  -------------------------
	
	override def toModel: Model = Model.from("text" -> text, "path" -> audioPath.toJson)
	
	
	// OTHER    -----------------------------
	
	/**
	 * Moves the audio file linked to this text to a new directory
	 * @param directory Directory to which the audio file will be placed
	 * @return A modified copy of this text
	 */
	def moveAudioTo(directory: Path) = audioPath.moveTo(directory).map { p => copy(audioPath = p) }
}
