package vf.readaloud.controller.audio

import utopia.flow.async.process.Breakable
import utopia.flow.util.TryExtensions._
import utopia.flow.view.mutable.async.Volatile
import vf.readaloud.model.audio.AudioFormat
import vf.readaloud.util.Common._

import javax.sound.sampled.{AudioSystem, SourceDataLine}
import scala.concurrent.Future
import scala.util.{Success, Try}

/**
 * Creates and manages shared [[javax.sound.sampled.SourceDataLine]] instances.
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
class AudioContext extends AutoCloseable with Breakable
{
	// ATTRIBUTES   --------------------------
	
	private val linesP = Volatile(Map[AudioFormat, SourceDataLine]())
	
	
	// IMPLEMENTED  -------------------------
	
	override def close() = linesP.getAndSet(Map()).valuesIterator.foreach { line =>
		Try { line.drain() }.logWithMessage("Failed to drain an audio line")
		Try { line.stop() }.logWithMessage("Failed to stop an audio line")
		Try { line.close() }.logWithMessage("Failed to close an audio line")
	}
	
	override def stop(): Future[Any] = Future { close() }
	
	
	// OTHER    -----------------------------
	
	/**
	 * Acquires a [[SourceDataLine]] supporting the specified audio format properties
	 * @param format Targeted audio format
	 * @return Source data line for playing that type of audio.
	 *         The line is already open.
	 *         Failure if no line could be acquired (due to resource, hardware or security reasons)
	 */
	// TODO: Later we probably just want to return a line wrapper, to prevent the user from closing it or something
	def lineFor(format: AudioFormat) = linesP.mutate { lines =>
		lines.get(format) match {
			// Case: A line has already been prepared
			case Some(existingLine) => Success(existingLine) -> lines
			// Case: No line available yet => Opens a new line
			case None =>
				val result = Try {
					val line = AudioSystem.getSourceDataLine(format.wrapped)
					line.open(format.wrapped)
					line.start()
					line
				}
				result -> (lines ++ result.toOption.map { format -> _ })
		}
	}
}
