package vf.readaloud.model.session

import utopia.flow.generic.model.immutable.{Model, ModelDeclaration}
import utopia.flow.generic.model.template.{ModelConvertible, ModelLike, Property}
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.factory.FromModelFactory
import utopia.flow.generic.model.mutable.DataType.{ModelType, StringType}
import utopia.flow.parse.file.FileExtensions._
import vf.readaloud.model.text.pdf.DocumentPosition

import java.nio.file.Path
import scala.util.Try

object SessionEndState extends FromModelFactory[SessionEndState]
{
	// ATTRIBUTES   ---------------------
	
	private lazy val schema = ModelDeclaration("document" -> StringType, "position" -> ModelType)
	
	
	// IMPLEMENTED  ---------------------
	
	override def apply(model: ModelLike[Property]): Try[SessionEndState] = schema.validate(model).flatMap { model =>
		DocumentPosition(model("position").getModel).map { position =>
			apply(model("document").getString, position)
		}
	}
}

/**
 * Documents the state at the end of a use session
 * @author Mikko Hilpinen
 * @since 01.10.2025, v0.1
 */
case class SessionEndState(documentDirectory: Path, position: DocumentPosition) extends ModelConvertible
{
	override def toModel: Model = Model.from("document" -> documentDirectory.toJson, "position" -> position)
}
