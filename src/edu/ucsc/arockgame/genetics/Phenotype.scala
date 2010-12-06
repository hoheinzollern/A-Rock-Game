package edu.ucsc.arockgame.genetics

import javax.sound.midi.Track

trait Phenotype {
	
	def getNote(note: String): Int = {
		val notes = Array("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
		var n = 0
		for (i <- 0 until notes.length)
			if (note.startsWith(notes(i))) n = i
		return ((note.charAt(notes(n).length)-'0')*12 + n)
	}
	
	def buildTrack(genotype: Genotype, track: Track, displacement: Int): Unit
}