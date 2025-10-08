package vf.readaloud.app.console

import utopia.echo.model.request.tts.piper.TtsParams
import utopia.flow.async.AsyncExtensions._
import utopia.flow.async.context.CloseHook
import utopia.flow.async.process.Delay
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.{Empty, Pair}
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
import vf.readaloud.controller.pdf.ReadPdf
import vf.readaloud.model.session.SessionEndState
import vf.readaloud.model.text.pdf.{DocumentPosition, SpokenPdfPage}
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
	private val dataDirectory: Path = "data"
	private val docDirectory: Path = dataDirectory/"documents"
	
	private val queuedPagesP = Pointer[(Seq[SpokenPdfPage], Option[Path])](Empty -> None)
	private val openDocumentDirP = Pointer.empty[Path]
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
								println(s"Reading $fileName...")
								val spokenDocumentResult = ReadPdf(targetFile).flatMapCatching { pages =>
									println(s"$fileName successfully read")
									// Asks the user to specify a name for this document
									val documentDirectory = {
										val defaultName = FileUtils.normalizeFileName(fileName.toLowerCase)
											.untilLast(".")
										val possiblyDuplicatePath = docDirectory /
											StdIn.readNonEmptyLine(
													s"What do you want to name this document? \nHint: Try not to include special characters\nDefault = $defaultName")
												.getOrElse(defaultName)
										
										if (possiblyDuplicatePath.exists) {
											if (StdIn.ask("A document with this name exists already. Do you want to overwrite it?")) {
												possiblyDuplicatePath.deleteContents()
													.logWithMessage("Failed to delete previous document files")
												possiblyDuplicatePath
											}
											else {
												val uniqueVersion = possiblyDuplicatePath.unique
												println(s"Named the document ${ uniqueVersion.fileName } instead")
												uniqueVersion
											}
										}
										else
											possiblyDuplicatePath
									}
									// Generates the audio
									documentDirectory.createDirectories().flatMapCatching { dir =>
										println(s"Preparing audio...")
										GenerateAudio.to(pages, dir/"audio").flatMap { spokenPages =>
											// Moves a copy of the original PDF to the directory
											val copyResult = targetFile.copyTo(dir)
											openDocumentDirP.setOne(dir)
											TryCatch.Success(dir -> spokenPages, copyResult.failure.emptyOrSingle)
										}
									}
								}
								
								spokenDocumentResult match {
									case TryCatch.Success((directory: Path, pages: Seq[SpokenPdfPage]), partialFailures) =>
										// Logs partial failures
										if (partialFailures.nonEmpty) {
											println(s"Encountered ${partialFailures.size} failures during content processing. The document may be incomplete.")
											log(partialFailures.head,
												s"This and ${ partialFailures.size - 1 } other failures during content processing")
										}
										
										println("Finalizing the document...")
										// Saves the document structure as JSON
										val saveResult = (directory/"structure").createDirectories()
											.flatMapCatching { dir =>
												pages.zipWithIndex
													.map { case (page, index) =>
														(dir/s"page-${ index + 1 }.json").writeJson(page)
													}
													.toTryCatch
											}
										saveResult match {
											case TryCatch.Success(_, partialFailures) =>
												if (partialFailures.nonEmpty) {
													log(partialFailures.head,
														s"This and ${ partialFailures.size - 1 } other failures during document structure -saving")
													println("The document structure couldn't be fully saved. The document may appear partial on consequent sessions.")
												}
											case TryCatch.Failure(error) =>
												log(error, "Couldn't save the document structure")
												println("Document structure couldn't be saved. You can only listen to this document during this session.")
										}
										
										// Queues the pages for faster listening
										queuedPagesP.value = pages -> Some(targetFile)
										
										println("The document is now ready to be listened. Use the \"listen\" command to listen it.")
									
									case TryCatch.Failure(error) => log(error, "Couldn't process the document")
								}
							}
					
				case Failure(error) => log(error, "Failed to scan for files")
			}
	}
	private val listenCommand = Command("listen", "play", help = "Listens through a previously processed document")(
		ArgumentSchema("document", help = "Name of the document to listen to (optional)")) {
		args =>
			// Selects the document to listen to
			val pagesToListen = {
				val docName = args("document").getString
				if (docName.isEmpty && queuedPagesP.value._1.nonEmpty &&
					StdIn.ask("Do you want to listen to the last processed document?", default = true))
					Some(Left(queuedPagesP.getAndSet(Empty -> None)))
				else {
					docDirectory
						.iterateChildren { filesIter =>
							if (docName.isEmpty)
								filesIter.toOptimizedSeq
							else
								filesIter.filter { _.fileName.containsIgnoreCase(docName) }.toOptimizedSeq
						}
						.logWithMessage("Couldn't look up existing documents")
						.flatMap { potentialDocPaths =>
							StdIn.selectFrom(potentialDocPaths.sortBy { _.fileName }.map { p => p -> p.fileName },
								"documents", "listen").map { Right(_) }
						}
				}
			}
			pagesToListen.foreach {
				case Left((pages, pdf)) => startListening(pdf, pages, None)
				case Right(directory) => startListening(directory)
			}
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
	Some(dataDirectory/"session.json").filter { _.exists }
		.flatMap { SessionEndState.fromPath(_).logWithMessage("Failed to load the previous session") }
		.filter { state => StdIn.ask(s"Do you want to continue listening ${ state.documentDirectory.fileName }?") }
		.foreach { state => startListening(state.documentDirectory, at = Some(state.position)) }
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
	
	private def saveState() = openDocumentDirP.pop().foreach { docDir =>
		(dataDirectory/"session.json").createDirectories()
			.flatMap { sessionPath =>
				sessionPath.writeJson(SessionEndState(docDir, narratorP.value match {
					case Some(narrator) => narrator.position
					case None => DocumentPosition.start
				}))
			}
			.logWithMessage("Failed to save the current document state")
	}
	
	private def startListening(docDirectory: Path, at: Option[DocumentPosition] = None): Unit = {
		openDocumentDirP.setOne(docDirectory)
		(docDirectory/"structure")
			.iterateChildren { pathsIter =>
				pathsIter.filter { _.fileType == "json" }.toVector.sortBy { _.fileName }
					.map { SpokenPdfPage.fromPath(_) }.toTryCatch
					.map { pages =>
						val pdf = docDirectory.iterateChildren { _.find { _.fileType == "pdf" } }
							.logWithMessage("Failed to scan for the PDF file").flatten
						pages -> pdf
					}
			}
			.flatMap { _.logToTryWithMessage("Failed to load some of the document pages") } match
		{
			case Success((pages, pdf)) => startListening(pdf, pages, at)
			case Failure(error) => log(error, "Couldn't load a document to listen to")
		}
	}
	
	private def startListening(pdf: Option[Path], pages: Seq[SpokenPdfPage], at: Option[DocumentPosition]) = {
		// Case: Successfully selected a document => Prepares and starts the narrator
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
			if (pdf.isDefined)
				Delay(1.seconds) { narrator.start() }
			else
				narrator.start()
			narratorP.setOne(narrator)
			println("You can control the narration with the \"pause\" and \"stop\" commands")
		}
		pdf.foreach { _.openInDesktop().logWithMessage("Failed to open the PDF") }
	}
}
