package vf.readaloud.controller.audio

import utopia.flow.async.AsyncExtensions._
import utopia.flow.time.Now
import utopia.flow.time.TimeExtensions._
import utopia.flow.util.TryExtensions._
import utopia.flow.util.logging.Logger
import utopia.flow.view.immutable.caching.Lazy
import utopia.flow.view.mutable.async.Volatile
import vf.readaloud.controller.audio.PlayWav._
import vf.readaloud.model.audio.AudioFormat

import java.io.BufferedInputStream
import java.nio.file.{Files, Path}
import java.time.Instant
import javax.sound.sampled.{AudioInputStream, AudioSystem}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object PlayWav
{
	// ATTRIBUTES   ------------------------
	
	private val kb = 1024
	
	private val streamBufferSize = 124 * kb
	
	private val defaultBufferSize = 0.07.seconds
	private val defaultPreparedBufferSize = 0.21.seconds
	
	
	// NESTED   ----------------------------
	
	private object PreparedAudio
	{
		def apply(stream: AudioInputStream): PreparedAudio = apply(Some(stream), stream.getFormat, None)
	}
	private case class PreparedAudio(stream: Option[AudioInputStream], format: AudioFormat,
	                                 preparedBuffer: Option[(Array[Byte], Int)])
}

/**
 * A stateful instance for playing the contents of a wav file once
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
class PlayWav(path: Path, bufferSize: FiniteDuration = defaultBufferSize)
             (implicit context: AudioContext, exc: ExecutionContext, log: Logger)
{
	// ATTRIBUTES   -------------------------
	
	private val preparedStreamP = Volatile.empty[Future[Try[PreparedAudio]]]
	private val completionP = Volatile.empty[Future[Try[Instant]]]
	
	
	// OTHER    ----------------------------
	
	/**
	 * Prepares this audio by buffering some of it in advance.
	 * Note: Only affects the next call to [[apply]], not the consequent calls.
	 * @return A lazily acquisible future that resolves once the buffering has completed
	 */
	def prepare(bufferSize: FiniteDuration = defaultPreparedBufferSize) = preparedStreamP.mutate {
		// Case: Prepare already called => Yields the same result
		case Some(existing) => Lazy { existing.map { _ => () } } -> Some(existing)
		case None =>
			val future = Future {
				// Acquires an input stream
				openStream().map { stream =>
					val format: AudioFormat = stream.getFormat
					
					// Buffers some of the input, so that the playback will be faster later
					val appliedBufferSize = bufferSizeFrom(format, bufferSize)
					val buffer = Try {
						val buffer = new Array[Byte](appliedBufferSize)
						val readByteCount = stream.read(buffer)
						Some(buffer -> readByteCount)
						
					}.getOrMap { error =>
						// If buffering failed, logs and attempts to proceed without one
						log(error, "Failure while preparing an audio buffer")
						None
					}
					
					// If the whole audio input fit into the prepared buffer, closes the stream
					val continuingStream = {
						if (buffer.exists { _._2 < appliedBufferSize }) {
							Try { stream.close() }.logWithMessage("Failed to close an audio stream")
							None
						}
						else
							Some(stream)
					}
					
					PreparedAudio(continuingStream, format, buffer)
				}
			}
			Lazy { future.map { _ => () } } -> Some(future)
	}
	
	/**
	 * Starts the playback of this sound
	 * @return A future that resolves once the full audio has been queued for playback.
	 *         Contains a timestamp of the estimated playback completion.
	 *         Yields a failure if the sound couldn't be (fully) queued.
	 * @see [[prepare]], which prepares this audio, so that the initial latency is lower
	 */
	// Checks for an already queued playback completion
	def apply() = completionP.mutate { queued =>
		queued.filter { _.isEmpty } match {
			// Case: Playback already in progress => Returns the previously queued completion
			case Some(queued) => queued -> Some(queued)
			case None =>
				// Consumes the prepared data, if available
				val completion = preparedStreamP.pop()
					.getOrElse { Future { openStream().map(PreparedAudio.apply) } }
					.tryMapIfSuccess { preparedAudio =>
						// Acquires the audio line
						val result = context.lineFor(preparedAudio.format).flatMap { line =>
							// Performs the playback
							Try {
								val startPosition = line.getLongFramePosition
								var bytesWritten = 0L
								
								// Writes the pre-buffered data, if applicable
								preparedAudio.preparedBuffer.foreach { case (buffer, bytesRead) =>
									line.write(buffer, 0, bytesRead)
									bytesWritten = bytesRead
								}
								
								// Consumes the input stream and writes it to the line
								preparedAudio.stream.foreach { stream =>
									val (buffer, appliedBufferSize) = preparedAudio.preparedBuffer match {
										// Case: A buffer was already constructed earlier => Continues to utilize it
										case Some((preparedBuffer, _)) => preparedBuffer -> preparedBuffer.length
										// Case: No data was buffered previously => Creates a new buffer
										case None =>
											val appliedBufferSize = bufferSizeFrom(preparedAudio.format, bufferSize)
											new Array[Byte](appliedBufferSize) -> appliedBufferSize
									}
									var streamContinues = true
									while (streamContinues) {
										val bytesRead = stream.read(buffer, 0, appliedBufferSize)
										if (bytesRead == -1)
											streamContinues = false
										else {
											line.write(buffer, 0, bytesRead)
											bytesWritten += bytesRead
										}
									}
								}
								
								// Calculates the remaining playback duration
								preparedAudio.format.frameSize.flatMap { frameSize =>
									preparedAudio.format.frameRate.map { frameSize -> _ }
								} match {
									case Some((frameSize, frameRate)) =>
										val now = Now.toInstant
										val currentPosition = line.getLongFramePosition
										val totalFrames = bytesWritten / frameSize
										val remainingFrames = (totalFrames - (currentPosition - startPosition)) max 0
										
										now + (remainingFrames / frameRate).seconds
									
									// Case: Playback duration can't be determined
									//       => Instead of calculating it, waits until the line is empty,
									//          and then returns
									case None =>
										line.drain()
										Now.toInstant
								}
							}
						}
						// Closes the audio stream
						preparedAudio.stream.foreach { stream =>
							Try { stream.close() }.logWithMessage("Failed to close an audio stream")
						}
						result
					}
				completion -> Some(completion)
		}
	}
	
	private def openStream() = {
		Try { new BufferedInputStream(Files.newInputStream(path), streamBufferSize) }.flatMap { stream =>
			val result = Try { AudioSystem.getAudioInputStream(stream) }
			
			// Case: Failed to open the audio stream => Closes the underlying file-input-stream
			if (result.isFailure)
				Try { stream.close() }
				
			result
		}
	}
	
	private def bufferSizeFrom(format: AudioFormat, length: FiniteDuration) = {
		format.byteRate match {
			case Some(bytesPerSecond) =>
				val raw = (length.toPreciseSeconds * bytesPerSecond).toInt
				// Rounds to the next kilobyte
				val remainder = raw % kb
				if (remainder == 0)
					raw
				else
					raw + (kb - remainder)
			
			case None => streamBufferSize
		}
	}
}
