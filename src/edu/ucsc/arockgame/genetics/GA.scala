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
	
	def getFittest(inv: Boolean) = {
		val f = if (inv)
			(a: Double, b:(Genotype,(Int,Double))) => a + (1d - b._2._2)
		else
			(a: Double, b:(Genotype,(Int,Double))) => a + b._2._2
		val tot = population.foldLeft(0d)(f)
		var target1 = random.nextFloat * tot
		var t1: Genotype = null
		val it = population.iterator
		while ((t1 == null) && it.hasNext) {
			val (genotype, (i, d)) = it.next
			if (t1 == null && target1 < d)
				t1 = genotype
			target1 -= d
		}
		(t1)
	}
	
	def getFittestPair(inv: Boolean) = {
		val t1 = getFittest(inv)
		var t2 = getFittest(inv)
		while (t1 == t2) {
			t2 = getFittest(inv)
		}
		(t1,t2)
	}
	
	def sigmoid(t: Double) = 1d / (1d + exp(-t/10))
	
	def evolve() = {
		val (a, b) = getFittestPair(false)
		val (e, f) = Genotype.breed(a, b)
		population(e) = (0, 0.5d)
		e
	}
	
	def generation() {
		val size = population.size
		var count = 0
		while (count < size/2) {
			population -= getFittest(true)
			count += 1
		}
		while (count >= 0) {
			evolve()
			count -= 1
		}
		
	}
}