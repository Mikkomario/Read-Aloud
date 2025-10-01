package vf.readaloud.util

import utopia.bunnymunch.jawn.JsonBunny
import utopia.echo.controller.client.PiperClient
import utopia.flow.async.context.ThreadPool
import utopia.flow.parse.json.JsonParser
import utopia.flow.util.logging.{Logger, SysErrLogger}

/**
 * Provides access to commonly used values
 * @author Mikko Hilpinen
 * @since 29.09.2025, v0.1
 */
object Common
{
	// ATTRIBUTES   -------------------------
	
	implicit val log: Logger = SysErrLogger
	implicit val exc: ThreadPool = new ThreadPool("Read-Aloud")
	implicit val jsonParser: JsonParser = JsonBunny
	
	implicit val piper: PiperClient = PiperClient()
}
