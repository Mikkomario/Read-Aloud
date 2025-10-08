package vf.readaloud.model.text.pdf

import utopia.flow.generic.model.immutable.{Model, ModelDeclaration}
import utopia.flow.generic.model.template.ModelConvertible
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.factory.FromModelFactoryWithSchema
import utopia.flow.generic.model.mutable.DataType.IntType

object DocumentPosition extends FromModelFactoryWithSchema[DocumentPosition]
{
	// ATTRIBUTES   --------------------------
	
	override lazy val schema: ModelDeclaration = ModelDeclaration("page" -> IntType, "section" -> IntType)
	
	/**
	 * The initial document position
	 */
	lazy val start = apply(0, 0, None)
	
	
	// IMPLEMENTED  --------------------------
	
	override protected def fromValidatedModel(model: Model): DocumentPosition =
		apply(model("page").getInt, model("section").getInt, model("paragraph").int)
}

/**
 * Used for representing a position within a (PDF) document
 * @author Mikko Hilpinen
 * @since 01.10.2025, v0.1
 */
case class DocumentPosition(pageIndex: Int, sectionIndex: Int = 0, paragraphIndex: Option[Int] = None)
	extends ModelConvertible
{
	override def toModel: Model =
		Model.from("page" -> pageIndex, "section" -> sectionIndex, "paragraph" -> paragraphIndex)
}