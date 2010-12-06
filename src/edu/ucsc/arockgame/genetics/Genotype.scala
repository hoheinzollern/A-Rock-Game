package edu.ucsc.arockgame.genetics

import scala.collection.immutable.BitSet
import java.util.Random
import java.math._
import javax.sound.midi._

class Genotype(val dna: BitSet, val len: Int) {
	
	override def toString: String = {
		def concat(i: Int, str: String): String = {
			if (i < len) {
				if (dna contains i) concat(i+1, str concat "1")
				else concat(i+1, str concat "0")
			} else str 
		}
		concat(0, "")
	}
	
	def buildTrack(track: Track): Unit = {}
}

object Genotype {
	val random = new Random
	
	def mutate(in: Genotype): Genotype = {
		var x = in
		for (i <- 0 until x.len) {
			if (random.nextDouble < 0.002) {
				if (x.dna contains i)
					x = Genotype(x.dna - i, x.len)
				else
					x = Genotype(x.dna + i, x.len)
			}
		}
		x
	}
	
	def breed(a: Genotype, b: Genotype) = {
		assert(a.len == b.len)
		val len = Math.max(a.len, b.len)
		
		// crossover
		val pivot = random.nextInt(len+1)
		var x = a.dna.filter(_ < pivot) | b.dna.filter(pivot <= _)
		var y = b.dna.filter(_ < pivot) | a.dna.filter(pivot <= _)
		
		// mutations
		(mutate(Genotype(x, len)), mutate(Genotype(y, len)))
	}
	
	def apply(dna: BitSet, len: Int) = new Genotype(dna, len)
	
	def apply(dna: String) = {
		var bs = BitSet()
		for (i <- 0 until dna.length) {
			if (dna.charAt(i)=='1')
				bs += i
		}
		new Genotype(bs, 128)
	}
}