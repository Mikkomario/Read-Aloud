package vf.readaloud.app.console

import utopia.echo.model.request.tts.piper.TtsParams
import utopia.flow.async.AsyncExtensions._
import utopia.flow.async.context.CloseHook
import utopia.flow.async.process.Delay
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.parse.file.FileUtils
import utopia.flow.time.TimeExtensions._
import utopia.flow.util.StringExtensions._
import utopia.flow.util.TryCatch
import utopia.flow.util.TryExtensions._
import utopia.flow.util.console.ConsoleExtensions._
import utopia.flow.util.console.{ArgumentSchema, Command, Console}
import utopia.flow.view.immutable.eventful.AlwaysFalse
import utopia.flow.view.mutable.Pointer
import utopia.flow.view.template.eventful.Flag
import vf.readaloud.controller.audio.{AudioContext, DocumentNarrator, GenerateAudio}
import vf.readaloud.controller.data.AudioDocuments
import vf.readaloud.model.document.AudioDocument
import vf.readaloud.model.document.pdf.DocumentPosition
import vf.readaloud.model.session.SessionEndState
import vf.readaloud.util.Common._

import java.nio.file.Path
import scala.io.StdIn
import scala.util.{Failure, Success}

/**
 * A simple console application for converting PDFs and reading them out aloud
 * @author Mikko Hilpinen
 * @since 30.09.2025, v0.1
 */
object ReadAloudConsole extends App
{
	// ATTRIBUTES   -----------------------
	
	private implicit val audioContext: AudioContext = new AudioContext()
	private implicit val ttsSettings: TtsParams = TtsParams.empty.fasterBy(0.05).withNoiseScale(0.6).withVariedTiming
	
	private val inputDirectory: Path = "input"
	
	private val lastDocumentP = Pointer.empty[AudioDocument]
	private val openDocumentP = Pointer.empty[AudioDocument]
	private val narratorP = Pointer.eventful.empty[DocumentNarrator]
	
	private val prepareCommand = Command("prepare", "make", help = "Prepares audio for a PDF document")(
		ArgumentSchema("file", help = "Name of the PDF file to convert. May be partial.")) {
		args =>
			// Looks for files to convert
			val potentialFiles = args("file").string match {
				case Some(searched) =>
					inputDirectory.iterateChildren { filesIter =>
						filesIter
							.filter { p =>
								val fileName = p.fileName
								fileName.endsWith("pdf") && fileName.containsIgnoreCase(searched)
							}
							.toOptimizedSeq
					}
				case None =>
					inputDirectory
						.iterateChildren { _.filter { _.fileName.endsWith("pdf") }.toVector }
			}
			potentialFiles match {
				case Success(files) =>
					// Case: No files found => Opens the input directory
					if (files.isEmpty) {
						println("No potential files were found from the input directory. Try again after adding some files.")
						inputDirectory.openInDesktop().logWithMessage("Couldn't open the input directory")
					}
					else
						StdIn.selectFrom(files.sortBy { _.fileName }.map { f => f -> f.fileName }, "files", "convert")
							.foreach { targetFile =>
								val fileName = targetFile.fileName
								val defaultDocName = FileUtils.normalizeFileName(fileName.toLowerCase).untilLast(".")
								val docName = StdIn.readNonEmptyLine(
										s"What do you want to name this document? \nHint: Try not to include special characters\nDefault = $defaultDocName")
									.getOrElse(defaultDocName)
								
								GenerateAudio.forDocument(AudioDocument.newDocument(docName, targetFile)) match {
									case TryCatch.Success(document: AudioDocument, failures) =>
										// Logs partial failures
										if (failures.nonEmpty) {
											println(s"Encountered ${failures.size} failures during content processing. The document may be incomplete.")
											log(failures.head,
												s"This and ${ failures.size - 1 } other failures during content processing")
										}
										AudioDocuments += document
										
										// Queues the pages for faster listening
										lastDocumentP.setOne(document)
										println("The document is now ready to be listened. Use the \"listen\" command to listen it.")
										
									case TryCatch.Failure(error) =>
										log(error, "Couldn't generate audio for the document")
								}
							}
					
				case Failure(error) => log(error, "Failed to scan for files")
			}
	}
	private val listenCommand = Command("listen", "play", help = "Listens through a previously processed document")(
		ArgumentSchema("document", help = "Name of the document to listen to (optional)")) {
		args =>
			// Selects the document to listen to
			val docToListen = {
				val docName = args("document").getString
				if (docName.isEmpty && lastDocumentP.nonEmpty &&
					StdIn.ask("Do you want to listen to the last processed document?", default = true))
					lastDocumentP.pop()
				else {
					val docs = AudioDocuments.value
					val docsToSelectFrom = {
						if (docName.nonEmpty)
							docs.filter { _.name.containsIgnoreCase(docName) }.notEmpty.getOrElse {
								println(s"No existing document matched \"$docName\"")
								docs
							}
						else
							docs
					}
					StdIn.selectFrom(docsToSelectFrom.sortBy { _.name }.map { p => p -> p.name }, "documents", "listen")
				}
			}
			docToListen.foreach { startListening(_) }
	}
	private val pauseCommand = Command.withoutArguments("pause", "p", help = "Pauses the narration") {
		if (narratorP.value.exists { _.pause() })
			println("The narration will pause at the end of this paragraph")
		else
			println("Already paused")
	}
	private val continueCommand = Command.withoutArguments("continue", help = "Continues the paused narration") {
		narratorP.value.foreach { narrator =>
			narrator.start()
			println("Continuing narration")
		}
	}
	private val stopCommand = Command.withoutArguments("stop", help = "Stops the narration and closes the document") {
		// Saves the state where we left off
		saveState()
		// Stops narration
		narratorP.pop().foreach { narrator =>
			println("Stopping the narration...")
			narrator.stop().foreach { _ => println("Narration stopped") }
		}
	}
	private val skipCommand = Command.withoutArguments("skip", help = "Skips straight to the next page") {
		narratorP.value.foreach { narrator =>
			narrator.goToNextPage() match {
				case Some(pageIndex) => println(s"Continues on page ${ pageIndex + 1 }")
				case None => println("Already at the last page")
			}
		}
	}
	private val goToPageCommand = Command("page", help = "Moves the narration to a specific page")(
		ArgumentSchema("page", help = "Number of the page to go to")) {
		args =>
			args("page").int match {
				case Some(pageIndex) =>
					narratorP.value.foreach { narrator =>
						narrator.moveTo(DocumentPosition(pageIndex - 1)) match {
							case Success(immediate) =>
								if (immediate)
									println(s"Now on page $pageIndex")
								else
									println(s"Goes to page $pageIndex after the current paragraph")
								
							case Failure(_) => println(s"$pageIndex is not a valid page index")
						}
					}
				case None => println("Try again with page index as the first parameter")
			}
	}
	
	private val hasNarratorFlag: Flag = narratorP.nonEmptyFlag
	private val pausedFlag: Flag = narratorP.flatMap {
		case Some(narrator) => narrator.pauseFlag
		case None => AlwaysFalse
	}
	
	private val commandsP = hasNarratorFlag.mergeWith(pausedFlag) { (narrating, paused) =>
		if (narrating) {
			val pauseOrContinue = if (paused) continueCommand else pauseCommand
			Vector(pauseOrContinue, stopCommand, skipCommand, goToPageCommand, prepareCommand)
		}
		else
			Pair(prepareCommand, listenCommand)
	}
	private val console = Console(commandsP,
		s"Please specify the next command: ${ commandsP.value.iterator.map { _.name }.mkString(" | ") } | help | exit",
		closeCommandName = "exit")
	
	
	// INITIAL CODE ------------------------
	
	audioContext.registerToStopOnceJVMCloses()
	
	// Loads the previous session, if appropriate
	Some(dataDirectory/"session.json").filter { _.exists }.foreach { path =>
		SessionEndState.fromPath(path).logWithMessage("Failed to load the previous session").foreach { state =>
			AudioDocuments.value.find { _.id == state.documentId }.foreach { doc =>
				if (StdIn.ask(s"Do you want to continue listening ${ doc.name }?"))
					startListening(doc, at = Some(state.position))
				else
					openDocumentP.setOne(doc)
			}
		}
	}
	// Before closing this software, makes sure the state is saved
	CloseHook.registerAction { saveState() }
	
	// Starts the console
	console.run()
	
	// Once the console closes, terminates the application
	println("Stopping...")
	narratorP.pop().foreach { _.stop().waitFor().logWithMessage("Unexpected failure while stopping the narrator") }
	audioContext.close()
	
	println("Bye!")
	System.exit(0)
	
	
	// OTHER    ---------------------------
	
	private def saveState() = openDocumentP.pop().foreach { doc =>
		(dataDirectory/"session.json").createDirectories()
			.flatMap { sessionPath =>
				sessionPath.writeJson(SessionEndState(doc.id, narratorP.value match {
					case Some(narrator) => narrator.position
					case None => DocumentPosition.start
				}))
			}
			.logWithMessage("Failed to save the current document state")
	}
	
	private def startListening(document: AudioDocument, at: Option[DocumentPosition] = None) = {
		document.pages match {
			case TryCatch.Success(pages, failures) =>
				openDocumentP.setOne(document)
				// Logs partial failures
				if (failures.nonEmpty) {
					println(s"Encountered ${failures.size} failures during content loading. The document may be incomplete.")
					log(failures.head,
						s"This and ${ failures.size - 1 } other failures during content loading")
				}
				if (pages.nonEmpty) {
					val narrator = new DocumentNarrator(pages)
					at.foreach { startingPosition =>
						println(s"Starting from page ${ startingPosition.pageIndex + 1 }/${ pages.size }${
							pages.lift(startingPosition.pageIndex) match {
								case Some(page) => page.pageHeader.prependIfNotEmpty(": ")
								case None => ""
							}
						}")
						narrator.position = startingPosition
					}
					Delay(1.seconds) { narrator.start() }
					narratorP.setOne(narrator)
					println("You can control the narration with the \"pause\" and \"stop\" commands")
				}
				document.openPdf().logWithMessage("Failed to open the PDF document")
			
			case TryCatch.Failure(error) => log(error, "Couldn't load the document's contents")
		}
	}
}
