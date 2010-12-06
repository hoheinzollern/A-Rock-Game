package edu.ucsc.arockgame.genetics

import javax.sound.midi._
import java.util.Random

object ChordPattern extends Phenotype {
	
	val random = new Random
	val ATTENUATION = 50
	
	// In the form of a tuple (pitch, displacement, velocity)
	val PROGRESSIONS = Array[Array[(Int, Int)]](
		Array(// G maj
			(getNote("G3"), 127),
			(getNote("B3"), 120),
			(getNote("D4"), 116),
			(getNote("B4"), 110),
			(getNote("D5"), 120),
			(getNote("G5"), 127)
		),
		Array(// A min
			(getNote("A3"), 127),
			(getNote("E4"), 120),
			(getNote("A4"), 110),
			(getNote("C5"), 120),
			(getNote("E5"), 127)
		),
		Array(// D maj
			(getNote("D4"), 127),
			(getNote("F#4"), 120),
			(getNote("D5"), 110),
			(getNote("F#5"), 120)
		),
		Array(// E maj
			(getNote("E3"), 127),
			(getNote("B3"), 120),
			(getNote("E4"), 110),
			(getNote("G#4"), 110),
			(getNote("B4"), 120),
			(getNote("E5"), 127)
		)
	)
	
	def chordOn(track: Track, index: Int, time: Int, up: Boolean) {
		var message: ShortMessage = null
		var event: MidiEvent = null
		var disp = PROGRESSIONS(index).length/2
		if (up) disp = -disp
		for ((note, vel) <- PROGRESSIONS(index)) {
			message = new ShortMessage
			message.setMessage(ShortMessage.NOTE_ON, 2, note, vel-ATTENUATION)
			event = new MidiEvent(message, time + disp)
			track.add(event)
			if (up) disp += 1 else disp -= 1
		}
	}
	
	def chordOff(track: Track, index: Int, time: Int) {
		var message: ShortMessage = null
		var event: MidiEvent = null
		for ((note, vel) <- PROGRESSIONS(index)) {
			message = new ShortMessage
			message.setMessage(ShortMessage.NOTE_OFF, 2, note, vel-ATTENUATION)
			event = new MidiEvent(message, time)
			track.add(event)
		}
	}
	
	def buildTrack(genotype: Genotype, track: Track) {
		var previous = -1
		var message: ShortMessage = new ShortMessage
		message.setMessage(ShortMessage.PROGRAM_CHANGE, 2, 25, 0)
		track.add(new MidiEvent(message, 0))
		var event: MidiEvent = null
		
		val dna = genotype.dna
		var up = true
		for (j <- 0 until 32) {
			val i = j * 4
			val chord = (if (dna contains i) 2 else 0) + (if (dna contains i+1) 1 else 0)
			val up = dna contains i+2
			val pause = (chord == 0) && !up && (dna contains i+3)
			val hold = (chord == 0) && !up && !pause
			if (!hold || previous == -1) {
				if (previous != -1 && previous != chord && !hold) {
					chordOff(track, previous, j * 16-3)
				}
				if (!pause || previous == -1)
					chordOn(track, chord, j * 16, up)
				previous = chord
			}
		}
	}
}