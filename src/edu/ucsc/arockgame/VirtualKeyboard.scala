package edu.ucsc.arockgame

import javax.swing._
import java.awt.event._

import javax.sound.midi._
import scala.collection.mutable.Set

class VirtualKeyboard extends JFrame with KeyListener {
	setSize(100, 100)
	setTitle("Scala Virtual Keyboard")
	setFocusable(true)
	addKeyListener(this)
	val display = new JLabel("", SwingConstants.CENTER)
	add(display)
	
	val receiver = MidiSystem.getReceiver
	var lastKeySet = Set[Int]()
	var octave = 0
	var noteMap = Map(
		'a' -> 48,
		'w' -> 49,
		's' -> 50,
		'e' -> 51,
		'd' -> 52,
		'f' -> 53,
		't' -> 54,
		'g' -> 55,
		'y' -> 56,
		'h' -> 57,
		'u' -> 58,
		'j' -> 59,
		'k' -> 60,
		'o' -> 61,
		'l' -> 62,
		'p' -> 63,
		';' -> 64,
		'\'' -> 65,
		']' -> 66,
		'\\' -> 67
	)
	val notes = Array("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
	
	override def keyReleased(e: KeyEvent) {
		try {
			val message = new ShortMessage
			message.setMessage(ShortMessage.NOTE_OFF, 1, noteMap(e.getKeyChar) + octave * 12, 127)
			receiver.send(message, -1)
			lastKeySet remove e.getKeyCode
			display setText ""
		} catch {
			case _:NoSuchElementException => 
		}
	}
	
	override def keyPressed(e: KeyEvent) {
		try {
			if (!lastKeySet.contains(e.getKeyCode)) {
				val message = new ShortMessage
				message.setMessage(ShortMessage.NOTE_ON, 1, noteMap(e.getKeyChar) + octave * 12, 127)
				receiver.send(message, -1)
				lastKeySet add e.getKeyCode
				display setText notes(noteMap(e.getKeyChar)%12)
			}
		} catch {
			case _:NoSuchElementException =>
		} 
	}
	
	override def keyTyped(e: KeyEvent) {
		e.getKeyChar match {
			case 'z' => octave = octave - 1
			case 'x' => octave = octave + 1
			case _ =>
		}
	}
}

object VirtualKeyboard {
	def main(args: Array[String]): Unit = {
		val vk = new VirtualKeyboard
		vk setVisible true
		vk setDefaultCloseOperation JFrame.EXIT_ON_CLOSE
	}
}