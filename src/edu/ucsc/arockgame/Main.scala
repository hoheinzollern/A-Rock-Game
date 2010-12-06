package edu.ucsc.arockgame

import genetics._
import java.io.File
import java.lang.Math
import java.util.Random
import scala.collection.immutable.BitSet
import javax.sound.midi._

object Main extends MetaEventListener {
	private val SAMPLE_RATE = 44100f
	private val BLOCK_SIZE = 4096
	private val AUDIO_THREAD = "Audio Thread"
	var sequencer = MidiSystem.getSequencer
	
	val chordGA = new GA()
	val drumGA = new GA()
	val melodyGA = new GA()
	drumGA.injectIndividual(Genotype(BitSet(0,2,4,6,8,10,12), 128))
	drumGA.injectIndividual(Genotype(BitSet(0,4,8,12), 128))
	drumGA.injectIndividual(Genotype(BitSet(0,2,4,6,8,10,12), 128))
	drumGA.injectIndividual(Genotype(BitSet(0,4,8,12), 128))
	drumGA.evolve
	chordGA.injectIndividual(Genotype(BitSet(0,2,4,6,8,10,12), 128))
	chordGA.injectIndividual(Genotype(BitSet(0,4,8,12), 128))
	chordGA.injectIndividual(Genotype(BitSet(0,4,8,12), 128))
	chordGA.injectIndividual(Genotype(BitSet(1,33,65,97), 128))
	chordGA.evolve
	melodyGA.injectIndividual(Genotype(BitSet(), 128))
	melodyGA.injectIndividual(Genotype(BitSet(), 128))
	melodyGA.injectIndividual(Genotype(BitSet(), 128))
	melodyGA.injectIndividual(Genotype(BitSet(), 128))
	melodyGA.evolve
	var (s,t) = chordGA.getFittestPair(false)
	
	def meta(message: MetaMessage) {
		if (message.getType == 47) {
			breed
		}
	}
    
	val random = new Random
    def breed {
    	val sequence = new Sequence(Sequence.PPQ, 64)
		
		val track = sequence.createTrack
		chordGA.evolve
		melodyGA.evolve
		drumGA.evolve
		
		val b = chordGA.getFittestPair(false)
		s = b._1
		t = b._2
		println(1+": fit:" + chordGA.population(s)._2)
		println(2+": fit:" + chordGA.population(t)._2)
		if (random.nextBoolean) {
			println("playing 1")
			ChordPattern.buildTrack(s, track, 0)
		} else {
			println("playing 2")
			ChordPattern.buildTrack(t, track, 0)
		}
    	//val melody = sequence.createTrack
    	//MelodyPattern.buildTrack(melodyGA.getFittestPair(false)._1, melody, 0)
    	val chords = sequence.createTrack
    	DrumPattern.buildTrack(drumGA.getFittestPair(false)._1, chords, 0)
		
		sequencer.setSequence(sequence)
		sequencer.setTempoInBPM(60)
		sequencer.start
    }
	
	def main(args: Array[String]) = {
		sequencer.addMetaEventListener(this)
		sequencer.open
		val PROGRAM = ShortMessage.PROGRAM_CHANGE
		
		breed
		var x = readLine
		while (!x.equals("q")) {
			try {
				var y = Integer.valueOf(x)
				if (y==1) {
					chordGA.voteUp(s)
				} else if (y==2) {
					chordGA.voteUp(t)
				}
			}
			x = readLine
		}
		System exit 0
	}
}