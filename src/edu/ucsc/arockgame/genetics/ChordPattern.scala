package edu.ucsc.arockgame.genetics

import javax.sound.midi._
import java.util.Random

object ChordPattern extends Phenotype {
	
	def getNote(note: String): Int = {
		val notes = Array("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
		var n = 0
		for (i <- 0 until notes.length)
			if (note.startsWith(notes(i))) n = i
		return ((note.charAt(notes(n).length)-'0')*12 + n)
	}
	
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
			println (note)
		}
	}
	
	def buildTrack(genotype: Genotype, track: Track) {
		var previous = -1
		var next = 0
		var message: ShortMessage = new ShortMessage
		message.setMessage(ShortMessage.PROGRAM_CHANGE, 2, 25, 0)
		track.add(new MidiEvent(message, 0))
		var event: MidiEvent = null
		next = random.nextInt(PROGRESSIONS.length)
		for (i <- 0 until 16) {
			if (previous != -1) {
				chordOff(track, previous, i * 16-3)
			}
			chordOn(track, next, i * 16, random.nextBoolean)
			previous = next
		}
	}
}