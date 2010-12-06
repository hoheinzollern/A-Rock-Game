package edu.ucsc.arockgame.genetics
import edu.ucsc.arockgame.genetics._
import javax.sound.midi._
import java.util.Random

object MelodyPattern extends Phenotype {
	val random = new Random
	
	val PENTATONIC = Array(
			0, 2, 4, 7, 9,
			12, 14, 16, 19, 21,
			24, 26, 28, 31, 33
	)
	
	def buildTrack(genotype: Genotype, track: Track) {
		var message: ShortMessage = new ShortMessage
		message.setMessage(ShortMessage.PROGRAM_CHANGE, 0, 1, 0)
		track.add(new MidiEvent(message, 0))
		for (i <- 0 until 16) {
			var event: MidiEvent = null
			val note = PENTATONIC(random.nextInt(PENTATONIC.length)) + 45
			
			message = new ShortMessage
			message.setMessage(ShortMessage.NOTE_ON, 1, note, 127)
			event = new MidiEvent(message, i*16)
			track.add(event)
			
			message = new ShortMessage
			message.setMessage(ShortMessage.NOTE_OFF, 1, note, 127)
			event = new MidiEvent(message, (i+1)*16)
			track.add(event)
		}
	}

}