package edu.ucsc.arockgame
import scala.collection.immutable.BitSet
import javax.sound.midi._

class DrumPattern(genotype: Genotype) {
	val ACOUSTIC_BASS = 35
	val ACOUSTIC_SNARE = 38
    val HAND_CLAP = 39
    val PEDAL_HIHAT = 44
    val LO_TOM = 45
    val CLOSED_HIHAT = 42
    val CRASH_CYMBAL1 = 49
    val HI_TOM = 50
    val RIDE_BELL = 53
    
    val INSTR = Array(ACOUSTIC_BASS, ACOUSTIC_SNARE, HAND_CLAP, PEDAL_HIHAT,
    		LO_TOM, CLOSED_HIHAT, CRASH_CYMBAL1, HI_TOM, RIDE_BELL)
	
	def buildTrack(track: Track): Unit = {
		var message: ShortMessage = null
		var event: MidiEvent = null
		for (i <- genotype.dna) {
			message = new ShortMessage
			message.setMessage(ShortMessage.NOTE_ON, 9, INSTR(i / 16), 127)
			event = new MidiEvent(message, i%16)
			track.add(event)
			
			message = new ShortMessage
			message.setMessage(ShortMessage.NOTE_OFF, 9, INSTR(i / 16), 127)
			event = new MidiEvent(message, i%16+1)
			track.add(event)
		}
	}
}

object DrumPattern {
	def apply(dna: Genotype) = new DrumPattern(dna)
}