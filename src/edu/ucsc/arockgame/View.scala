package edu.ucsc.arockgame

import javax.swing._
import javax.sound.midi._
import java.awt._
import java.awt.event._
import genetics._
import scala.collection.immutable.BitSet

class PhenotypeCanvas extends Canvas {
	var genotype: Genotype = null
	
	def setGenotype(genotype: Genotype) {
		this.genotype = genotype
		repaint
	}
}

class DrumCanvas extends PhenotypeCanvas {
	setSize(128, 64)
	
	override def paint(g: Graphics) {
		if (genotype == null) return
		g.setColor(Color.BLUE)
		val dna = genotype.dna
		for (i <- 0 until 16) {
			for (j <- 0 until 8) {
				if (dna contains (i + j * 16))
					g.fillRect(i * 8, j * 8, 8, 8)
			}
		}
	}
}

class ChordCanvas extends PhenotypeCanvas {
	setSize(128, 64)
	
	override def paint(g: Graphics) {
		if (genotype == null) return
		g.setColor(Color.GREEN)
		val dna = genotype.dna
		var last = -1
		for (i <- 0 until 16) {
			val chord = (if (dna contains i) 2 else 0) + (if (dna contains i+1) 1 else 0)
			val up = dna contains i+2
			val pause = (chord == 0) && !up && (dna contains i+3)
			val hold = (chord == 0) && !up && !pause
			if (last != -1 && hold)
				g.fillRect(i * 8, last * 8, 8, 8)
			else if (!pause) {
				last = -1
			} else {
				g.fillRect(i * 8, chord * 8, 8, 8)
			}
		}
	}
}

class MelodyCanvas extends PhenotypeCanvas {
	setSize(128, 64)
	
	override def paint(g: Graphics) {
		if (genotype == null) return
		g.setColor(Color.RED)
		val dna = genotype.dna
		var previous = -1
		for (i <- 0 until 16) {
			val j = i * 4
			var k = (if (dna contains j) 8 else 0) + (if (dna contains j+1) 4 else 0) +
					(if (dna contains j+2) 2 else 0) + (if (dna contains j+3) 1 else 0)
			if (k == 15)
				k = previous
			if (k != 14)
				g.fillRect(i * 8, k * 4, 8, 4)
			previous = k
		}
	}
}

class GenotypeView(ga: GA) extends JPanel {
	setLayout(new GridLayout(4,1))
	setSize(128,192)
	val pc = Array[PhenotypeCanvas](null, null, null)
	val commands = new Commands
	def currentIndex = commands.sel.getSelectedIndex
	
	class Commands extends JPanel {
		val sel = new JComboBox()
		sel.addItem("1")
		sel.addItem("2")
		sel.addItem("3")
		val del = new JButton("Delete")
		class DelActList extends ActionListener {
			override def actionPerformed(event: ActionEvent) {
				val index = currentIndex
				val genotype = pc(index).genotype
				
				pc(index).setGenotype(ga.evolve)
				ga.population -= genotype
			}
		}
		del.addActionListener(new DelActList)
		val comb = new JButton("Combine")
		class CombActList extends ActionListener {
			override def actionPerformed(event: ActionEvent) {
				ga.generation
				val it = ga.population.iterator
				pc(0).setGenotype(it.next._1)
				pc(1).setGenotype(it.next._1)
				pc(2).setGenotype(it.next._1)
			}
		}
		comb.addActionListener(new CombActList)
		setLayout(new FlowLayout)
		add (sel)
		add (del)
		add (comb)
	}
	
}

class DrumView(val ga: GA) extends GenotypeView(ga) {
	pc(0) = new DrumCanvas
	pc(1) = new DrumCanvas
	pc(2) = new DrumCanvas

	add(pc(0))
	add(pc(1))
	add(pc(2))
	add (commands)
	
	var gt = Genotype("10101010100010001000000000000000100001001000100001000100010010001000100010100101001011000000000000000000000000000000000000000000")
	pc(0).setGenotype(gt)
	ga.injectIndividual(gt)
	gt = Genotype("00000000000000010000100100010000100010001001000100010001010010100101100000000000000000000000000000000000000000010101010100010001")
	pc(1).setGenotype(gt)
	ga.injectIndividual(gt)
	gt = Genotype("10001001000100010001010010100101100000000000000000000000000000000000000000010101010100010001000000000000000100001001000100001000")
	pc(2).setGenotype(gt)
	ga.injectIndividual(gt)
	
}

class ChordView(val ga: GA) extends GenotypeView(ga) {
	pc(0) = new ChordCanvas
	pc(1) = new ChordCanvas
	pc(2) = new ChordCanvas

	add(pc(0))
	add(pc(1))
	add(pc(2))
	add (commands)
	
	var gt = Genotype("10101010100010001000000000000000100001001000100001000100010010001000100010100101001011000000000000000000000000000000000000000000")
	pc(0).setGenotype(gt)
	ga.injectIndividual(gt)
	gt = Genotype("00000000000000010000100100010000100010001001000100010001010010100101100000000000000000000000000000000000000000010101010100010001")
	pc(1).setGenotype(gt)
	ga.injectIndividual(gt)
	gt = Genotype("10001001000100010001010010100101100000000000000000000000000000000000000000010101010100010001000000000000000100001001000100001000")
	pc(2).setGenotype(gt)
	ga.injectIndividual(gt)
}

class MelodyView(val ga: GA) extends GenotypeView(ga) {
	pc(0) = new MelodyCanvas
	pc(1) = new MelodyCanvas
	pc(2) = new MelodyCanvas

	add(pc(0))
	add(pc(1))
	add(pc(2))
	add (commands)
	
	var gt = Genotype("10101010100010001000000000000000100001001000100001000100010010001000100010100101001011000000000000000000000000000000000000000000")
	pc(0).setGenotype(gt)
	ga.injectIndividual(gt)
	gt = Genotype("00000000000000010000100100010000100010001001000100010001010010100101100000000000000000000000000000000000000000010101010100010001")
	pc(1).setGenotype(gt)
	ga.injectIndividual(gt)
	gt = Genotype("10001001000100010001010010100101100000000000000000000000000000000000000000010101010100010001000000000000000100001001000100001000")
	pc(2).setGenotype(gt)
	ga.injectIndividual(gt)
}

class MelodyRec extends JDialog with KeyListener {
	setTitle("Memory recording")
	setLayout(new FlowLayout)
	setModal(true)
	setSize(500, 300)
	setFocusable(true)
	addKeyListener(this)
	val canvas = new MelodyCanvas
	add(canvas)
	
	var genotype: Genotype = new Genotype(BitSet(), 128)
	var i = 0
	val noteMap = Map(
		'1' -> 0,
		'2' -> 1,
		'3' -> 2,
		'4' -> 3,
		'5' -> 4,
		'q' -> 5,
		'w' -> 6,
		'e' -> 7,
		'r' -> 8,
		't' -> 9,
		'a' -> 10,
		's' -> 11,
		'd' -> 12,
		'f' -> 13,
		' ' -> 15
	)
	
	def getMelody = genotype
	
	var lastKp: Char = '.'
	var count = 0
	
	override def keyReleased(e: KeyEvent) {
		try {
			val x = noteMap(lastKp)
			if ((x & 8) != 0)
				genotype = Genotype(genotype.dna + i, genotype.len)
			if ((x & 4) != 0)
				genotype = Genotype(genotype.dna + (i + 1), genotype.len)
			if ((x & 2) != 0)
				genotype = Genotype(genotype.dna + (i + 2), genotype.len)
			if ((x & 1) != 0)
				genotype = Genotype(genotype.dna + (i + 3), genotype.len)
			i += 4
			count -= 1
			while (count > 0) {
				genotype = Genotype(genotype.dna + i, genotype.len)
				genotype = Genotype(genotype.dna + i + 1, genotype.len)
				genotype = Genotype(genotype.dna + i + 2, genotype.len)
				i += 4
				count -= 1
			}
			canvas.setGenotype(genotype)
			lastKp = '.'
		} catch {
			case _ =>
		}
	}
	
	override def keyPressed(e: KeyEvent) {
		lastKp = e.getKeyChar
	}
	
	override def keyTyped(e: KeyEvent) {
		count += 1
	}
}

class View extends JFrame {
	val drumGA = new GA
	val chordGA = new GA
	val melodyGA = new GA
	setTitle("A Rock Game")
	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
	setLayout(new FlowLayout)
	setSize(700,400)
	val dv = new DrumView(drumGA)
	add(dv)
	
	val cv = new ChordView(chordGA)
	add(cv)
	
	val mv = new MelodyView(melodyGA)
	add(mv)
	
	val insertMelody = new JButton("Record melody")
	class InsertMelodyListener extends ActionListener {
		override def actionPerformed(action: ActionEvent) {
			val mr = new MelodyRec
			mr.setVisible(true)
			val genotype = mr.getMelody
			val index = mv.currentIndex
			melodyGA.population -= mv.pc(index).genotype
			mv.pc(index).setGenotype(genotype)
			melodyGA.injectIndividual(genotype)
		}
	}
	insertMelody.addActionListener(new InsertMelodyListener)
	add(insertMelody)

	val play = new JButton("Play patterns")
	class PlayListener extends ActionListener {
		override def actionPerformed(action: ActionEvent) {
			val sequencer = MidiSystem.getSequencer
			sequencer.open
			val sequence = new Sequence(Sequence.PPQ, 64)
			val drumTrack = sequence.createTrack
			val drumGT = dv.pc(dv.currentIndex).genotype
			DrumPattern.buildTrack(drumGT, drumTrack, 0)
			drumGA.voteUp(drumGT)
			val chordTrack = sequence.createTrack
			val chordGT = cv.pc(cv.currentIndex).genotype
			ChordPattern.buildTrack(chordGT, chordTrack, 0)
			val melodyTrack = sequence.createTrack
			val melodyGT = mv.pc(mv.currentIndex).genotype
			MelodyPattern.buildTrack(melodyGT, melodyTrack, 0)
			
			sequencer.setSequence(sequence)
			sequencer.setTempoInBPM(60)
			sequencer.start
		}
	}
	play.addActionListener(new PlayListener)
	add(play)
}

object View {
	def main(args: Array[String]) {
		val view = new View
		view.setVisible(true)
	}
}
