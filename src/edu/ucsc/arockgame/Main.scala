package edu.ucsc.arockgame

import java.io.File
import java.lang.Math
import com.synthbot.audioplugin.vst.vst2.JVstHost2
import com.synthbot.audioio.vst.JVstAudioThread

import javax.sound.midi.ShortMessage

object Main {
	private val SAMPLE_RATE = 44100f
	private val BLOCK_SIZE = 4096
	private val AUDIO_THREAD = "Audio Thread"
	
	def play(vst: JVstHost2, note: Int, time: Int) {
		val midiMessage = new ShortMessage
		midiMessage.setMessage(ShortMessage.NOTE_ON, 0, note, 127)
		vst.queueMidiMessage(midiMessage)
		Thread.sleep(time)
		midiMessage.setMessage(ShortMessage.NOTE_OFF, 0, note, 127)
		vst.queueMidiMessage(midiMessage)
	}
	
	def main(args: Array[String]) = {
		val vstFile = new File("MinimogueVA.dll")
		val vst = JVstHost2.newInstance(vstFile, SAMPLE_RATE, BLOCK_SIZE)
		
		val audioThread = new JVstAudioThread(vst)
		val thread = new Thread(audioThread)
		thread.setName(AUDIO_THREAD)
		thread.setDaemon(true)
		thread.start()
		while (true) {
			play(vst, 48, 250)
			play(vst, 48, 250)
			play(vst, 48, 500)
			play(vst, 48, 500)
			play(vst, 48, 500)
		}
	}
}