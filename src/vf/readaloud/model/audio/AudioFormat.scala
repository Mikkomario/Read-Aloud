package vf.readaloud.model.audio

import utopia.flow.operator.equality.EqualsBy
import vf.readaloud.model.audio.AudioFormat.JAudioFormat

import javax.sound.sampled.AudioFormat.Encoding
import javax.sound.sampled.AudioSystem
import scala.language.implicitConversions

object AudioFormat
{
	// TYPES    ------------------------
	
	type JAudioFormat = javax.sound.sampled.AudioFormat
	
	
	// IMPLICIT ------------------------
	
	implicit def wrap(format: JAudioFormat): AudioFormat = new AudioFormat(format)
}

/**
 * An interface used for comparing audio formats
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
class AudioFormat(val wrapped: JAudioFormat) extends EqualsBy
{
	// ATTRIBUTES   --------------------
	
	/**
	 * @return The number of samples taken per second.
	 *         sample consists of [[sampleSizeInBits]] bits for each included channel.
	 */
	val sampleRate = Some(wrapped.getSampleRate).filter { _ != AudioSystem.NOT_SPECIFIED }
	/**
	 * @return Size of an individual sample, in bits
	 */
	val sampleSizeInBits = Some(wrapped.getSampleSizeInBits).filter { _ != AudioSystem.NOT_SPECIFIED }
	/**
	 * @return The number of channels
	 */
	val channelCount = Some(wrapped.getChannels).filter { _ != AudioSystem.NOT_SPECIFIED }
	
	override protected lazy val equalsProperties: Seq[Any] =
		Vector(encoding, sampleRate, sampleSizeInBits, channelCount, isBigEndian)
	
	/**
	 * Size of a single frame, in bytes.
	 * A frame consists of a single sample for each targeted channel.
	 */
	lazy val frameSize = Some(wrapped.getFrameSize).filter { _ != AudioSystem.NOT_SPECIFIED }.orElse {
		sampleSizeInBits.flatMap { sampleSizeBits =>
			channelCount.map { channels => sampleSizeBits / 8 * channels }
		}
	}
	/**
	 * Number of frames advanced every second, when using this format
	 */
	lazy val frameRate = Some(wrapped.getFrameRate).filter { _ != AudioSystem.NOT_SPECIFIED }.orElse(sampleRate)
	/**
	 * Number of bytes of audio consumed / advanced every second, when using this format
	 */
	lazy val byteRate = frameSize.flatMap { size => frameRate.map { size * _ } }
	
	
	// COMPUTED ------------------------
	
	/**
	 * @return The type of encoding for sounds in this format.
	 */
	def encoding: Encoding = wrapped.getEncoding
	
	/**
	 * @return Whether the audio data is stored in a big endian byte order.
	 *         False if stored in little endian.
	 */
	def isBigEndian: Boolean = wrapped.isBigEndian
	
	
	// IMPLEMENTED  ---------------------
	
	override def canEqual(a: Any) = a.isInstanceOf[AudioFormat] || a.isInstanceOf[AudioFormat]
	
	override def equals(a: Any) = a match {
		case props: AudioFormat =>
			encoding == props.encoding && sampleRate == props.sampleRate && sampleSizeInBits == props.sampleSizeInBits &&
				channelCount == props.channelCount && isBigEndian == props.isBigEndian
		
		case format: JAudioFormat =>
			encoding == format.getEncoding && sampleRate.getOrElse(AudioSystem.NOT_SPECIFIED) == format.getSampleRate &&
				sampleSizeInBits.getOrElse(AudioSystem.NOT_SPECIFIED) == format.getSampleSizeInBits &&
				channelCount.getOrElse(AudioSystem.NOT_SPECIFIED) == format.getChannels &&
				isBigEndian == format.isBigEndian
			
		case _ => false
	}
}
