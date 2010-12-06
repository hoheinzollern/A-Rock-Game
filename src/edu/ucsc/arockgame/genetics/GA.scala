package edu.ucsc.arockgame.genetics

import scala.collection.mutable.Map
import java.util.Random
import scala.Math._

class GA {
	var population = Map[Genotype, (Int, Double)]()
	val random = new Random
	
	def injectIndividual(individual: Genotype) {
		population(individual) = (0, 0.5d) 
	}
	
	def voteUp(individual: Genotype) {
		var (count, prob) = population(individual)
		population(individual) = (count+1, sigmoid(count+1))
	}
	
	def voteDown(individual: Genotype) {
		var (count, prob) = population(individual)
		population(individual) = (count-1, sigmoid(count-1))
	}
	
	def getFittestPair(inv: Boolean) = {
		val f = if (inv)
			(a: Double, b:(Genotype,(Int,Double))) => a + (1d - b._2._2)
		else
			(a: Double, b:(Genotype,(Int,Double))) => a + b._2._2
		val tot = population.foldLeft(0d)(f)
		var target1 = random.nextFloat * tot
		var target2 = random.nextFloat * tot
		var t1: Genotype = null
		var t2: Genotype = null
		var partial = 0d
		val it = population.iterator
		while ((t1 == null || t2 == null) && it.hasNext) {
			val oldPartial = partial
			val (genotype, (i, d)) = it.next
			partial += (if (inv) (1-d) else (d))
			if (oldPartial <= target1 && target1 < partial)
				t1 = genotype
			if (oldPartial <= target2 && target2 < partial)
				t2 = genotype
			if(t1 == t2 && t2 != null)
				target2 = partial
		}
		(t1, t2)
	}
	
	def sigmoid(t: Double) = 1d / (1d + exp(-t/10))
	
	def evolve() {
		val (a, b) = getFittestPair(false)
		val (c, d) = getFittestPair(true)
		val (e, f) = Genotype.breed(a, b)
		population -= c
		population -= d
		population(e) = (0, 0.5d)
		population(f) = (0, 0.5d)
	}
}