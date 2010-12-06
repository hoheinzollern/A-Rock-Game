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
					g.fillRect(i * 8, j * 8, i * 8 + 7, j * 8 + 7)
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
		for (i <- 0 until 16) {
			val chord = (if (dna contains i) 2 else 0) + (if (dna contains i+1) 1 else 0)
			g.fillRect(i * 8, chord * 8, i * 8 + 8, chord * 8 + 8)
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
				g.fillRect(i * 8, j * 4, (i * 8) + 8, (j * 4) + 4)
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
}

class ChordView(val ga: GA) extends GenotypeView(ga) {
	pc(0) = new ChordCanvas
	pc(1) = new ChordCanvas
	pc(2) = new ChordCanvas

	add(pc(0))
	add(pc(1))
	add(pc(2))
	pc(0).setGenotype(new Genotype(BitSet(0,8,64,92), 128))
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

class View extends JFrame {
	val drumGA = new GA
	setTitle("A Rock Game")
	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
	setLayout(new GridLayout(1,3))
	setSize(128*3,192)
	add(new DrumView(drumGA))
	add(new ChordView(drumGA))
	add(new MelodyView(drumGA))
}

object View {
	def main(args: Array[String]) {
		val view = new View
		view.setVisible(true)
	}
}
