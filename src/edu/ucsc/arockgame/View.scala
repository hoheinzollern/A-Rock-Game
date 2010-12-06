package edu.ucsc.arockgame

import javax.swing._
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
	setSize(128, 32)
	
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
	setLayout(new GridLayout(3,1))
	setSize(128,192)
	val pc = Array[PhenotypeCanvas](null, null, null)
}

class DrumView(val ga: GA) extends GenotypeView(ga) {
	pc(0) = new DrumCanvas
	pc(1) = new DrumCanvas
	pc(2) = new DrumCanvas

	add(pc(0))
	add(pc(1))
	add(pc(2))
	pc(0).setGenotype(new Genotype(BitSet(0,8,64,92), 128))
	pc(1).setGenotype(new Genotype(BitSet(0,8,64,92), 128))
	pc(2).setGenotype(new Genotype(BitSet(0,8,64,92), 128))
}

class ChordView(val ga: GA) extends GenotypeView(ga) {
	pc(0) = new ChordCanvas
	pc(1) = new ChordCanvas
	pc(2) = new ChordCanvas

	add(pc(0))
	add(pc(1))
	add(pc(2))
	pc(0).setGenotype(new Genotype(BitSet(0,8,64,92), 128))
	pc(1).setGenotype(new Genotype(BitSet(0,8,64,92), 128))
	pc(2).setGenotype(new Genotype(BitSet(0,8,64,92), 128))
}

class MelodyView(val ga: GA) extends GenotypeView(ga) {
	pc(0) = new MelodyCanvas
	pc(1) = new MelodyCanvas
	pc(2) = new MelodyCanvas

	add(pc(0))
	add(pc(1))
	add(pc(2))
	pc(0).setGenotype(new Genotype(BitSet(0,8,64,92), 128))
	pc(1).setGenotype(new Genotype(BitSet(0,8,64,92), 128))
	pc(2).setGenotype(new Genotype(BitSet(0,8,64,92), 128))
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
	
	def getMelody() {
		return genotype
	}
	
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
	setTitle("A Rock Game")
	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
	setLayout(new FlowLayout)
	setSize(500,300)
	add(new DrumView(drumGA))
	add(new ChordView(drumGA))
	add(new MelodyView(drumGA))
	
	val insertMelody = new JButton("Record melody")
	class InsertMelodyListener extends ActionListener {
		override def actionPerformed(action: ActionEvent) {
			val mr = new MelodyRec
			mr.setVisible(true)
		}
	}
	insertMelody.addActionListener(new InsertMelodyListener)
	add(insertMelody)

	val play = new JButton("Play patterns")
	class PlayListener extends ActionListener {
		override def actionPerformed(action: ActionEvent) {
			// TODO: do the play stuff
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
