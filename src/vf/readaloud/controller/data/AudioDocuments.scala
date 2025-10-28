package vf.readaloud.controller.data

import utopia.flow.parse.file.container.ObjectsFileContainer
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.view.immutable.View
import vf.readaloud.model.document.AudioDocument
import vf.readaloud.util.Common._

/**
 * Stores audio documents
 * @author Mikko Hilpinen
 * @since 27.10.2025, v1.0
 */
object AudioDocuments extends View[Seq[AudioDocument]]
{
	// ATTRIBUTES   --------------------------
	
	private lazy val container = new ObjectsFileContainer(dataDirectory/"docs.json", AudioDocument)
	
	
	// IMPLEMENTED  --------------------------
	
	override def value: Seq[AudioDocument] = container.current
	
	
	// OTHER    ------------------------------
	
	/**
	 * Stores an audio document
	 * @param document The document to store or update
	 */
	def +=(document: AudioDocument) = container.pointer.update { _.filterNot { _.id == document.id } :+ document }
}
