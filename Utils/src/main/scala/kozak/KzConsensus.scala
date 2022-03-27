package org.vulpine_designs.initiator_set.utils
package kozak

import mRNA._
import scala.language.implicitConversions

/**
 * This stores the list or sequence of [[KzNucleotide]], along with a few helper
 * methods and data, allowing for a list of [[mRNA.Bases]] to be compared against it to
 * determine if the desired sequence has been found.
 *
 * ==Example==
 * You can compare a list of [[mRNA.Bases]] against the consensus by using [[KzConsensus.similarity]]
 * this returns a double denoting the "strength" of whether or not the sequence is
 * the desired one;
 *
 * {{{
 *     val similarityScore = consensus.similarity(listOfBases)
 * }}}
 *
 * @param CodonStart: `Int` the stating position of the 3 element long codon within [[KzConsensus.Sequence]].
 *
 * @param Sequence: `List[KzNucleotide]` a list of [[KzNucleotide]] holding information about each
 * position within the sequence.
 */
class KzConsensus(
		val CodonStart: Int,
		val Sequence: List[KzNucleotide]
) {
	val SimilarityThreshold: Int = -1
	val ConservedThreshold: Int = -1
	
	/**
	 * Gets the positions specified as being the codon (defined by [[KzConsensus.CodonStart]]),
	 * then returns the dominant base (the base with the highest score within [[KzNucleotide.NucleotideDict]])
	 * for each position.
	 *
	 * @return `List[Bases]` the list of dominant bases in the specified codon position.
	 */
	def codon: List[MRNABases] = {
		val kzNucCodon = Sequence.slice(CodonStart, CodonStart + 3)
		kzNucCodon map { nucleotide =>
			nucleotide.dominantNucleotideDist.head._1
		}
	}
	
	/**
	 * Gets the positions specified as being the codon (defined by [[KzConsensus.CodonStart]]),
	 * then returns the dominant base (the base with the highest score within [[KzNucleotide.NucleotideDict]])
	 * for each position, and tries to index it via [[mRNA.indexCodon]].
	 *
	 * @return `List[Bases]` the list of dominant bases in the specified codon position.
	 */
	def indexedCodon: Option[Int] = {
		val kzNucCodon = Sequence.slice(CodonStart, CodonStart + 3)
		val unIndexedCodon = kzNucCodon map { nucleotide =>
			nucleotide.dominantNucleotideDist.head._1
		}
		indexCodon(unIndexedCodon)
	}
	
	/**
	 * Gets the highest weight for each of the [[KzConsensus.Sequence]] positions, defined in
	 * [[KzNucleotide.NucleotideDict]] multiplied by [[KzNucleotide.Importance]], and sums them.
	 *
	 * @return `Double` the sum of all the weights * all the importances.
	 */
	def totalWeight: Double = {
		val weights = Sequence map { nucleotide =>
			val weight = nucleotide.maximumNucleotideDist.head._2
			weight * nucleotide.Importance
		}
		weights.sum
	}
	
	/**
	 * Takes a list of [[mRNA.Bases]], and gets the weight for each of them in their position compared to
	 * [[KzConsensus.Sequence]] and returns them in a list, a greater number of high weights
	 * suggests that the correct sequence has been found.
	 *
	 * @param comparisonSeq: `List[Bases]` the list of bases to be compared.
	 *
	 * @param comparisonStart `Int` (optional) where to start checking comparisonSeq.
	 *
	 * @return `List[Double]` a list of weights corresponding the the comparisonSeq.
	 */
	def similarityDistribution(comparisonSeq: List[MRNABases], comparisonStart: Int = 0): List[Double] = {
		val comparisonLength = comparisonSeq.length - comparisonStart
		val rangeEnd = if(Sequence.length > comparisonLength)
			comparisonLength
			else Sequence.length
		val confidenceRes = 0 to rangeEnd map { iter =>
			val currentKzNucleotide = Sequence(iter)
			val comparisonBase = comparisonSeq(comparisonStart + iter)
			currentKzNucleotide.getConfidence(comparisonBase)
		}
		confidenceRes.toList
	}
	
	/**
	 * Takes a list of [[mRNA.Bases]], and gets the weight for each of them in their position compared to
	 * [[KzConsensus.Sequence]] and returns the sum of them all, a greater sum suggests that the
	 * correct sequence has been found.
	 *
	 * @param comparisonSeq: `List[Bases]` the list of bases to be compared.
	 *
	 * @param comparisonStart `Int` (optional) where to start checking comparisonSeq.
	 *
	 * @return `Double` the sum of all the weights in the comparisonSeq.
	 */
	def similarityTally(comparisonSeq: List[MRNABases], comparisonStart: Int = 0): Double =
		similarityDistribution(comparisonSeq, comparisonStart).sum
	
	/**
	 * Takes a list of [[mRNA.Bases]], and gets the weight for each of them in their position compared to
	 * [[KzConsensus.Sequence]] and returns the sum of them all, a greater sum suggests that the
	 * correct sequence has been found, will be 0 if below the value specified in [[KzConsensus.SimilarityThreshold]].
	 *
	 * @param comparisonSeq: `List[Bases]` the list of bases to be compared.
	 *
	 * @param comparisonStart `Int` (optional) where to start checking comparisonSeq.
	 *
	 * @return `Double` the sum of all the weights in the comparisonSeq.
	 */
	def similarity(comparisonSeq: List[MRNABases], comparisonStart: Int = 0): Double = {
		val similarityDist = similarityDistribution(comparisonSeq, comparisonStart)
		if(similarityDist.contains(-1))
			return 0
		similarityDist.sum / totalWeight
	}
	
	/**
	 * Takes a list of [[mRNA.Bases]], and gets the weight for each of them in their position compared to
	 * [[KzConsensus.Sequence]] and returns true if the sum of them all is above [[KzConsensus.SimilarityThreshold]]
	 * and false if below.
	 *
	 * @param comparisonSeq: `List[Bases]` the list of bases to be compared.
	 *
	 * @param comparisonStart `Int` (optional) where to start checking comparisonSeq.
	 *
	 * @return `Boolean` whether or not the som of the weights in the comparisonSeq is greater than
	 * [[KzConsensus.SimilarityThreshold]].
	 */
	def homologous(comparisonSeq: List[MRNABases], comparisonStart: Int = 0): Boolean =
		similarity(comparisonSeq, comparisonStart) >= SimilarityThreshold
}

/**
 * Assosciated object for [[KzConsensus]], contains implicit type casts.
 */
object KzConsensus {
	implicit def toString(kzConsensus: KzConsensus): String = {
		val nucleotidesStrings = kzConsensus.Sequence map { nucleotide =>
			val nucString: String = nucleotide
			if(nucleotide.Importance >= kzConsensus.ConservedThreshold)
				nucString.toUpperCase()
			else nucString
		}
		nucleotidesStrings.mkString
	}
}

