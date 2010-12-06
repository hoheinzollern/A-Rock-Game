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
		if (target2 < target1) {
			val temp = target1
			target1 = target2
			target2 = temp
		}
		var t1: Genotype = null
		var t2: Genotype = null
		val it = population.iterator
		while ((t1 == null || t2 == null) && it.hasNext) {
			val (genotype, (i, d)) = it.next
			if (t1 == null && target1 < d)
				t1 = genotype
			if (t2 == null && target2 < d)
				t2 = genotype
			target1 -= d
			target2 -= d
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