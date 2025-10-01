package vf.readaloud.test

import utopia.flow.async.AsyncExtensions._
import utopia.flow.async.process.Wait
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.time.Now
import utopia.flow.time.TimeExtensions._
import vf.readaloud.controller.audio.{AudioContext, PlayWav}
import vf.readaloud.util.Common._

import java.nio.file.Paths

/**
 * Tests playing of 2 wav files sequentially
 * @author Mikko Hilpinen
 * @since 30.09.2025, v0.1
 */
object PlayWavTest extends App
{
	private implicit val audioContext: AudioContext = new AudioContext
	private val audio = Paths.get("data/test-data/audio")
		.iterateChildren { _.filter { _.fileType == "wav" }.nextPair() }.get.map { new PlayWav(_) }
	
	println("Preparing audio...")
	private val prepareStartTime = Now.toInstant
	private val prepareFinishTimes = audio.map { audio =>
		audio.prepare().value.waitFor()
		Now.toInstant
	}
	println(s"Preparations took ${ (prepareFinishTimes.first - prepareStartTime).description } and ${
		(prepareFinishTimes.second - prepareFinishTimes.first).description }")
	
	private def play(audio: PlayWav) = {
		println("Playing the audio...")
		val startTime = Now.toInstant
		val estimatedCompletion = audio().waitForResult().get
		val bufferedTime = Now.toInstant
		println(s"Audio buffered in ${
			(bufferedTime - startTime).description }; The playback is estimated to finish in ${
			(estimatedCompletion - bufferedTime).description }")
		println("Waiting for the audio to finish...")
		Wait(estimatedCompletion)
		println("Finished playing the audio")
	}
	
	println("\nStarting with the first audio")
	play(audio.first)
	
	println("\nMoving on to the second audio")
	play(audio.second)
	
	println("Done!")
}
