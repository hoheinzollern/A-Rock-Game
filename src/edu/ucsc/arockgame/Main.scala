package edu.ucsc.arockgame

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
	val x = Genotype(BitSet(0,2,4,6,8,12,14), 16)
	val y = Genotype(BitSet(0,4,8,12), 16)
	var (s,t) = Genotype.breed(x,y)
	
	def meta(message: MetaMessage) {
		if (message.getType == 47) {
			breed
		}
	}
	
    def createEvent(track: Track, evtType: Int, chan: Int, num: Int, tick: Long): Unit = {
        val message = new ShortMessage()
        try {
            message.setMessage(evtType, chan, num, 127) 
            val event = new MidiEvent( message, tick )
            track.add(event)
        } catch { case ex => ex.printStackTrace() }
    }
    
	val random = new Random
    def breed {
    	val sequence = new Sequence(Sequence.PPQ, 16)
		val track = sequence.createTrack
		
		val b = Genotype.breed(s,t)
		s = b._1
		t = b._2
		if (random.nextBoolean)
			DrumPattern(s).buildTrack(track)
		else
			DrumPattern(t).buildTrack(track)
		sequencer.setSequence(sequence)
		
		sequencer.setTempoInBPM(30)
		sequencer.start
    }
	
	def main(args: Array[String]) = {
		sequencer.addMetaEventListener(this)
		sequencer.open
		val PROGRAM = ShortMessage.PROGRAM_CHANGE
		
		breed
		while (!readLine.equals("q")) {
			
		}
		System exit 0
	}
}