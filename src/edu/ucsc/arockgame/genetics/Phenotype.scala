package edu.ucsc.arockgame.genetics

import javax.sound.midi.Track

trait Phenotype {
	def buildTrack(genotype: Genotype, track: Track): Unit
}