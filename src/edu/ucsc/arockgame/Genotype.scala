package edu.ucsc.arockgame

import scala.collection.immutable.BitSet
import java.util.Random
import java.math._

class Genotype(val dna: BitSet, val len: Int) {
	def getPhenotype: Phenotype = null
	
	override def toString: String = {
		def concat(i: Int, str: String): String = {
			if (i < len) {
				if (dna contains i) concat(i+1, str concat "1")
				else concat(i+1, str concat "0")
			} else str 
		}
		concat(0, "")
	}
}

object Genotype {
	val random = new Random
	
	def mutate(in: Genotype): Genotype = {
		var x = in
		for (i <- 0 until x.len) {
			if (random.nextDouble < 0.001) {
				if (x.dna contains i)
					x = Genotype(x.dna - i, x.len)
				else
					x = Genotype(x.dna + i, x.len)
			}
		}
		x
	}
	
	def breed(a: Genotype, b: Genotype): (Genotype, Genotype) = {
		assert(a.len == b.len)
		val len = Math.max(a.len, b.len)
		
		// crossover
		val pivot = random.nextInt(len+1)
		var x = Genotype(a.dna.filter(_ < pivot) | b.dna.filter(pivot <= _), len)
		var y = Genotype(b.dna.filter(_ < pivot) | a.dna.filter(pivot <= _), len)
		
		// mutations
		(mutate(x), mutate(y))
	}
	
	def apply(dna: BitSet, len: Int) = new Genotype(dna, len)
}