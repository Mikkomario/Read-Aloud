# Read-Aloud
Read-Aloud is an application that reads PDF documents to you out loud.

Note, that this software is pretty much in its beta-phase, only providing the most necessary features.

## Purpose
The purpose of this software is to help students access documents in audio format, 
where no audiobooks or other resources are readily available.

## License & Acknowledgements
You're free to use, to modify, distribute and to sell this software, with a few basic requirements:
- Don't go around claiming you created this software, when you didn't
- You accept the fact that this product comes without any warranty - You choose to use it at your own risk. 
  I won't be liable if you manage to break something with it, or if it doesn't work as you thought it would.
- If you use this software to do something unlawful, you'll be solely responsible for your own actions

If you're considering modifying this software, I'd also recommend considering collaboration with myself.

Also, this software uses Apache PDFBox and Apache HttpClient. 
These are distributed under Apache 2.0 license.

The JSON parsing used in this project is based on the [Jawn](https://github.com/typelevel/jawn) library.

## Installation & Usage

### Requirements
This software requires a locally running [Piper](https://github.com/OHF-Voice/piper1-gpl) http server to function. 
Check the link above for installation instructions.

You'll also need to have Java 8 or later installed.

## Installation
Download the zip file from the Releases section and unzip it somewhere.

## Usage
1. Add one or more PDF files to the `input` directory
2. In terminal, go to the unzipped software directory and run: `java -jar Read-Aloud.jar`
3. Use the `prepare` command to convert the PDF into audio files
4. Use the `listen` command to start listening to the PDF
5. Use `pause`, `continue` and `stop` commands to control the playback
6. Close the software with `exit`