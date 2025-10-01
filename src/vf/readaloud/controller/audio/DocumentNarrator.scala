package vf.readaloud.controller.audio

import utopia.flow.async.AsyncExtensions._
import utopia.flow.async.TryFuture
import utopia.flow.async.process.WaitTarget.{NoWait, Until}
import utopia.flow.async.process.{Breakable, LoopingProcess, Wait}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.time.Now
import utopia.flow.time.TimeExtensions._
import utopia.flow.view.mutable.eventful.ResettableFlag
import utopia.flow.view.mutable.{Pointer, Resettable}
import vf.readaloud.model.text.pdf.{DocumentPosition, SpokenPdfPage}
import vf.readaloud.util.Common._

import java.time.Instant
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

/**
 * Narrates a document by speaking it out loud.
 * @author Mikko Hilpinen
 * @since 30.09.2025, v0.1
 */
// NB: Assumes that pages is non-empty
// TODO: Extend HasProcessState, once that's available
class DocumentNarrator(pages: Seq[SpokenPdfPage],
                       paragraphPause: FiniteDuration = 1.seconds, sectionPause: FiniteDuration = 2.seconds,
                       headerPause: FiniteDuration = 1.seconds, pagePause: FiniteDuration = 0.5.seconds)
                      (implicit audioContext: AudioContext)
	extends Resettable with Breakable
{
	// ATTRIBUTES   ---------------------------
	
	private val pageIndexP = Pointer.eventful(0)
	private val sectionIndexP = Pointer.eventful(0)
	private val paragraphIndexP = Pointer.eventful.empty[Int]
	
	private val queuedPositionP = Pointer.empty[DocumentPosition]
	private val updatingPositionFlag = ResettableFlag()
	private lazy val positionP = pageIndexP.mergeWithWhile(sectionIndexP, paragraphIndexP, !updatingPositionFlag) {
		(page, section, paragraph) => DocumentPosition(page, section, paragraph)
	}
	
	private val preparedAudioP = Pointer.empty[PlayWav]
	private var lastPlaybackCompletionFuture: Future[Try[Instant]] = TryFuture.success(Now.toInstant)
	
	private val _pauseFlag = ResettableFlag()
	
	private val loop = LoopingProcess() { hurryFlag =>
		// Case: Paused during the wait => Won't continue playing for now
		if (_pauseFlag.isSet)
			None
		else {
			// Identifies and plays the next audio (blocks)
			val playbackFuture = preparedAudioP.pop().getOrElse { new PlayWav(currentText.audioPath) }.apply()
			lastPlaybackCompletionFuture = playbackFuture
			// Case: Requested to hurry => Won't wait for the audio to fully queue
			if (hurryFlag.value)
				None
			else
				playbackFuture.waitForResult() match {
					case Success(scheduledCompletion) =>
						// Advances the "cursor" or applies a queued position update
						val nextText = queuedPositionP.pop() match {
							case Some(nextPosition) =>
								applyPosition(nextPosition)
								Some(currentText -> sectionPause)
							case None => advance()
						}
						nextText.flatMap { case (nextText, pause) =>
							val nextAudio = preparedAudioP.setOne(new PlayWav(nextText.audioPath))
							
							// Case: Narration paused => Won't schedule the next audio
							if (_pauseFlag.isSet)
								None
							else {
								// Schedules the next audio playback
								val nextAudioTime = scheduledCompletion + pause
								
								// Prepares the audio, if there is time
								if (nextAudioTime.isFuture) {
									nextAudio.prepare()
									Some(Until(nextAudioTime))
								}
								else
									Some(NoWait)
							}
						}
					// Case: Audio playback failed => Pauses
					case Failure(error) =>
						log(error, "Failure during audio playback => Terminates")
						_pauseFlag.set()
						None
				}
		}
	}
	
	/**
	 * A mutable pointer that pauses or resumes this narrator
	 */
	lazy val pauseFlag = ResettableFlag.wrap(Pointer.indirect(_pauseFlag) { newState =>
		if (_pauseFlag.value != newState) {
			if (newState)
				start()
			else
				pause()
		}
	})
	
	
	// COMPUTED ----------------------------
	
	/**
	 * @return A pointer that contains this narrator's current state
	 * @see [[pauseFlag]]
	 */
	def statePointer = loop.statePointer
	
	/**
	 * @return This narrator's current position in the document
	 */
	def position = positionP.value
	def position_=(newPosition: DocumentPosition) = moveTo(newPosition)
	
	private def pageIndex = pageIndexP.value
	private def currentPage = pages(pageIndex)
	
	private def sectionIndex = sectionIndexP.value
	private def currentSection = currentPage.sections(sectionIndex)
	
	private def paragraphIndex = paragraphIndexP.value
	
	private def currentText = {
		val section = currentSection
		paragraphIndex match {
			case Some(index) => section.paragraphs(index)
			case None => section.header.getOrElse(section.paragraphs.head)
		}
	}
	
	
	// IMPLEMENTED  ----------------------
	
	override def isSet: Boolean = paragraphIndex.nonEmpty || sectionIndex > 0 || pageIndex > 0
	
	override def reset(): Boolean = {
		if (isSet) {
			paragraphIndexP.clear()
			sectionIndexP.value = 0
			pageIndexP.value = 0
			true
		}
		else
			false
	}
	
	override def stop(): Future[Any] = {
		// Requests the loop to stop
		val stopFuture = loop.stopIfRunning()
		// Resets back to the original position
		reset()
		// Yields a future that resolves once the sound has fully stopped
		stopFuture.flatMap { _ =>
			lastPlaybackCompletionFuture.flatMap {
				case Success(completionTime) =>
					if (completionTime.isFuture)
						Future { Wait(Until(completionTime)) }
					else
						Future.successful(())
				
				case Failure(_) => Future.successful(())
			}
		}
	}
	
	
	// OTHER    --------------------------
	
	/**
	 * Starts or continues narrating the document
	 */
	def start() = {
		// Unpauses, if paused
		_pauseFlag.reset()
		if (loop.isStartable) {
			// Before starting, makes sure the previous audio has fully played out before continuing
			lastPlaybackCompletionFuture.waitForResult().toOption.foreach { completion =>
				Wait(Until(completion + paragraphPause))
			}
			loop.runAsync()
		}
	}
	/**
	 * Pauses the narration at the end of the current paragraph
	 * @return Whether this call had any effect. False if already paused, or if not narrating at this time.
	 */
	def pause() = _pauseFlag.set() && loop.state.isRunning
	
	/**
	 * Moves this narrator to a specific position in the document
	 * @param nextPosition New position in the document
	 * @return Failure if the specified position was out-of-bounds.
	 *         False if the position will update after a delay. True if the position was updated immediately.
	 */
	def moveTo(nextPosition: DocumentPosition) = {
		// Makes sure the position is valid
		pages.lift(nextPosition.pageIndex)
			.toTry { new IndexOutOfBoundsException(s"Page index ${ nextPosition.pageIndex } is out of bounds (0-${
				pages.size - 1 })") }
			.flatMap { page =>
				page.sections.lift(nextPosition.sectionIndex)
					.toTry { new IndexOutOfBoundsException(s"Section index ${
						nextPosition.sectionIndex } is out of bounds (0-${ page.sections.size - 1 })") }
					.flatMap { section =>
						if (nextPosition.paragraphIndex.exists { i => i < 0 || i >= section.paragraphs.size })
							Failure(new IndexOutOfBoundsException(s"Paragraph index ${
								nextPosition.paragraphIndex } is out of bounds (0-${ section.paragraphs.size - 1 })"))
						// Case: Currently narrating another section
						//       => Queues the position update instead of applying it immediately
						else if (loop.state.isRunning) {
							queuedPositionP.setOne(nextPosition)
							Success(false)
						}
						// Case: Not narrating at this time => Applies the position update immediately
						else {
							applyPosition(nextPosition)
							Success(true)
						}
					}
			}
	}
	
	/**
	 * Advances the current text pointers to point to the next piece of text.
	 * If reached the end of the document, resets the pointers back to their initial positions
	 * (i.e. to point to the first text entry)
	 * @return The next piece of text. None if the end of the document was reached.
	 */
	private def advance() = {
		updatingPositionFlag.set()
		val result = pageIndexP.mutate { pageIndex =>
			val openPage = pages(pageIndex)
			val sections = openPage.sections
			// Attempts to find the next text entry on the currently open page
			val nextTextOnPage = sectionIndexP.mutate { sectionIndex =>
				val openSection = sections(sectionIndex)
				val paragraphs = openSection.paragraphs
				// Attempts to find the next paragraph on the currently open section
				val nextParagraph = paragraphIndexP.mutate {
					case Some(paragraphIndex) =>
						val nextIndex = paragraphIndex + 1
						// Case: Reached the end of this section => Resets the paragraph pointer
						if (paragraphs.hasSize(nextIndex))
							None -> None
						// Case: In the middle of this section
						//       => Yields the next paragraph & updates the paragraph pointer
						else
							Some(paragraphs(nextIndex) -> paragraphPause) -> Some(nextIndex)
					
					// Case: At the beginning of this section
					case None =>
						// Case: This section only contained a header => No paragraphs to play
						if (paragraphs.isEmpty)
							None -> None
						// Case: The first played entry was a paragraph => Looks for the 2nd paragraph
						else if (openSection.header.isEmpty) {
							// Case: 2nd paragraph found => Advances to it
							if (paragraphs.hasSize > 1)
								Some(paragraphs(1) -> paragraphPause) -> Some(1)
							// Case: No 2nd paragraph => Resets
							else
								None -> None
						}
						// Case: The first entry was the header => Moves to the first paragraph
						else
							Some(paragraphs.head -> headerPause) -> Some(0)
				}
				// Case: Next paragraph found from this section => Resolves
				if (nextParagraph.isDefined)
					nextParagraph -> sectionIndex
				// Case: Last section was finished => No more text on this page
				else if (sections.hasSize(sectionIndex + 1))
					None -> 0
				// Case: More sections remain => Moves to the beginning of the next section
				else {
					val nextIndex = sectionIndex + 1
					val section = sections(nextIndex)
					Some(section.header.getOrElse { section.paragraphs.head } -> sectionPause) -> nextIndex
				}
			}
			// Case: Text found on this page => Resolves
			if (nextTextOnPage.isDefined)
				nextTextOnPage -> pageIndex
			// Case: Last page was fully read => Resets without yielding any text
			else if (pages.hasSize(pageIndex + 1))
				None -> 0
			// Case: Next page available => Starts from the beginning of that page
			else {
				val nextIndex = pageIndex + 1
				val section = pages(nextIndex).sections.head
				Some(section.header.getOrElse { section.paragraphs.head } -> pagePause) -> nextIndex
			}
		}
		updatingPositionFlag.reset()
		result
	}
	
	private def applyPosition(position: DocumentPosition) = {
		updatingPositionFlag.set()
		pageIndexP.value = position.pageIndex
		sectionIndexP.value = position.sectionIndex
		paragraphIndexP.value = position.paragraphIndex
		updatingPositionFlag.reset()
	}
}
