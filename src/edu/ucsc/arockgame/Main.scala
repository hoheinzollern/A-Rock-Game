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
	chordGA.injectIndividual(Genotype(BitSet(0,2,4,6,8,10,12), 128))
	chordGA.injectIndividual(Genotype(BitSet(0,4,8,12), 128))
	chordGA.evolve
	var (s,t) = chordGA.getFittestPair(false)
	
	def meta(message: MetaMessage) {
		if (message.getType == 47) {
			breed
		}
	}
    
	val random = new Random
    def breed {
		try{
    	val sequence = new Sequence(Sequence.PPQ, 64)
		
		val track = sequence.createTrack
		for (i <- 0 to 100)
			chordGA.evolve
		
		val b = chordGA.getFittestPair(false)
		s = b._1
		t = b._2
		println(1+": fit:" + chordGA.population(s)._2)
		println(2+": fit:" + chordGA.population(t)._2)
		if (random.nextBoolean) {
			println("playing 1")
			DrumPattern.buildTrack(s, track)
			chordGA.voteUp(s)
		} else {
			println("playing 2")
			DrumPattern.buildTrack(t, track)
			chordGA.voteUp(t)
		}
    	val melody = sequence.createTrack
    	MelodyPattern.buildTrack(t, melody)
    	val chords = sequence.createTrack
    	ChordPattern.buildTrack(t, chords)
		
		sequencer.setSequence(sequence)
		sequencer.setTempoInBPM(60)
		sequencer.start
		} catch {
			case e => println (e)
		}
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