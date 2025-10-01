package vf.readaloud.test

import utopia.echo.model.request.tts.piper.TtsParams
import utopia.flow.async.AsyncExtensions._
import utopia.flow.async.process.Wait
import utopia.flow.collection.immutable.Single
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.time.TimeExtensions._
import vf.readaloud.controller.audio.{AudioContext, DocumentNarrator, GenerateAudio}
import vf.readaloud.controller.pdf.ReadPdf
import vf.readaloud.util.Common._

import java.nio.file.Paths

/**
 * @author Mikko Hilpinen
 * @since 30.09.2025, v0.1
 */
object AudioSettingsTest extends App
{
	private implicit val audioContext: AudioContext = new AudioContext
	// Reads the source document first
	private val page = ReadPdf(Paths.get("input").iterateChildren { _.find { _.fileType == "pdf" } }.get.get)
		.get.head
	
	// Converts it to audio using various settings
	Vector(
		TtsParams.empty.withNoiseScale(0.4) -> "consistent",
		TtsParams.empty.fasterBy(0.1).moreConsistentTone.withVariedTiming -> "faster")
		.foreach { case (params, name) =>
			implicit val p: TtsParams = params
			println(s"Generating the $name version...")
			val spoken = GenerateAudio.to(Single(page), "data/test-data/output/audio", name).get
			println(s"Playing the $name version...")
			val narrator = new DocumentNarrator(spoken)
			narrator.start()
			Wait(10.seconds)
			narrator.stop().waitFor()
		}
	
	println("Finishing...")
	audioContext.close()
}
