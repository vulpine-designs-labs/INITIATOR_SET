package org.vulpinedesigns.initiator_set
package utils.kozak

import utils.mrna.{
	MRNABases, 
	indexCodon
}
import scala.language.implicitConversions


/**
 * This stores the list or sequence of [[utils.kozak.KzNucleotide KzNucleotide]], along with a few helper
 * methods and data, allowing for a list of [[utils.mrna.MRNABases MRNABases]] to be compared against it to
 * determine if the desired sequence has been found.
 *
 * ==Example==
 * You can compare a list of [[utils.mrna.MRNABases MRNABases]] against the consensus by using
 * [[utils.kozak.KzConsensus.similarity() KzConsensus.similarity]] this returns a double denoting the
 * "strength" of whether or not the sequence is the desired one;
 *
 * {{{
 *     val similarityScore = consensus.similarity(listOfBases)
 * }}}
 *
 * @param CodonStart The stating position of the 3 element long codon within
 *                   [[utils.kozak.KzConsensus.Sequence KzConsensus.Sequence]].
 * @param Sequence A list of [[utils.kozak.KzNucleotide KzNucleotide]] holding information about each
 * position within the sequence.
 */
class KzConsensus(
     val CodonStart: Int,
     val Sequence: List[KzNucleotide]
) {
	/**
	 * Used when comparing a similarity in (e.g. [[utils.kozak.KzConsensus.similarity KzConsensus.similarity ]])
	 * as the minimum threshold for if a comparison is similar or not.
	 */
	val SimilarityThreshold: Int = -1
	
	/**
	 * Used for identifying conserved [[utils.kozak.KzNucleotide KzNucleotide]] (ones that rarely change) within
	 * [[utils.kozak.KzConsensus KzConsensus.Sequence]] based on [[utils.kozak.KzNucleotide.Importance KzNucleotide.Importance]].
	 */
	val ConservedThreshold: Int = -1
	
	/**
	 * Gets the positions specified as being the codon (defined by [[utils.kozak.KzConsensus.CodonStart KzConsensus.CodonStart]]),
	 * then returns the dominant base (the base with the highest score within
	 * [[utils.kozak.KzNucleotide.NucleotideDict KzNucleotide.NucleotideDict]]) for each position.
	 *
	 * @return The list of dominant bases in the specified codon position.
	 */
	def codon: List[MRNABases] = {
		val kzNucCodon = Sequence.slice(CodonStart, CodonStart + 3)
		kzNucCodon map { nucleotide =>
			nucleotide.dominantNucleotideDist.head._1
		}
	}
	
	/**
	 * Gets the positions specified as being the codon (defined by [[utils.kozak.KzConsensus.CodonStart KzConsensus.CodonStart]]),
	 * then returns the dominant base (the base with the highest score within
	 * [[utils.kozak.KzNucleotide.NucleotideDict KzNucleotide.NucleotideDict]]) for each position, and tries
	 * to index it via [[utils.mrna.indexCodon() indexCodon]].
	 *
	 * @return The list of dominant bases in the specified codon position.
	 */
	def indexedCodon: Option[Int] = {
		val kzNucCodon = Sequence.slice(CodonStart, CodonStart + 3)
		val unIndexedCodon = kzNucCodon map { nucleotide =>
			nucleotide.dominantNucleotideDist.head._1
		}
		indexCodon(unIndexedCodon)
	}
	
	/**
	 * Gets the highest weight for each of the [[utils.kozak.KzConsensus.Sequence KzConsensus.Sequence]] positions, defined in
	 * [[utils.kozak.KzNucleotide.NucleotideDict KzNucleotide.NucleotideDict]] multiplied by
	 * [[utils.kozak.KzNucleotide.Importance KzNucleotide.Importance]], and sums them.
	 *
	 * @return The sum of all the weights * all the importances.
	 */
	def totalWeight: Double = {
		val weights = Sequence map { nucleotide =>
			val weight = nucleotide.maximumNucleotideDist.head._2
			weight * nucleotide.Importance
		}
		weights.sum
	}
	
	/**
	 * Takes a list of [[utils.mrna.MRNABases MRNABases]], and gets the weight for each of them in 
	 * their position compared to [[utils.kozak.KzConsensus.Sequence KzConsensus.Sequence]] and returns them in a list,
	 * a greater number of high weights suggests that the correct sequence has been found.
	 *
	 * @param comparisonSeq The list of bases to be compared.
	 * @param comparisonStart The (optional) index to start checking comparisonSeq.
	 * @return A list of weights corresponding the the comparisonSeq.
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
	 * Takes a list of [[utils.mrna.MRNABases MRNABases]], and gets the weight for each of them in their position compared to
	 * [[utils.kozak.KzConsensus.Sequence KzConsensus.Sequence]] and returns the sum of them all, a greater sum suggests that the
	 * correct sequence has been found.
	 *
	 * @param comparisonSeq The list of bases to be compared.
	 * @param comparisonStart The (optional) index to start checking comparisonSeq.
	 * @return The sum of all the weights in the comparisonSeq.
	 */
	def similarityTally(comparisonSeq: List[MRNABases], comparisonStart: Int = 0): Double =
		similarityDistribution(comparisonSeq, comparisonStart).sum
	
	/**
	 * Takes a list of [[utils.mrna.MRNABases MRNABases]], and gets the weight for each of them in their position compared to
	 * [[utils.kozak.KzConsensus.Sequence KzConsensus.Sequence]] and returns the sum of them all,
	 * a greater sum suggests that the correct sequence has been found, will be 0 if below the value
	 * specified in [[utils.kozak.KzConsensus.SimilarityThreshold KzConsensus.SimilarityThreshold]].
	 *
	 * @param comparisonSeq The list of bases to be compared.
	 * @param comparisonStart The (optional) index to start checking comparisonSeq.
	 * @return The sum of all the weights in the comparisonSeq.
	 */
	def similarity(comparisonSeq: List[MRNABases], comparisonStart: Int = 0): Double = {
		val similarityDist = similarityDistribution(comparisonSeq, comparisonStart)
		if(similarityDist.contains(-1))
			return 0
		similarityDist.sum / totalWeight
	}
	
	/**
	 * Takes a list of [[utils.mrna.MRNABases MRNABases]], and gets the weight for each of them in their position compared to
	 * [[utils.kozak.KzConsensus.Sequence KzConsensus.Sequence]] and returns true if the sum of them all is above
	 * [[utils.kozak.KzConsensus.SimilarityThreshold KzConsensus.SimilarityThreshold]] and false if below.
	 *
	 * @param comparisonSeq The list of bases to be compared.
	 * @param comparisonStart The (optional) index where to start checking comparisonSeq.
	 * @return Whether or not the som of the weights in the comparisonSeq is greater than
	 *         [[utils.kozak.KzConsensus.SimilarityThreshold KzConsensus.SimilarityThreshold]].
	 */
	def homologous(comparisonSeq: List[MRNABases], comparisonStart: Int = 0): Boolean =
		similarity(comparisonSeq, comparisonStart) >= SimilarityThreshold
}

/**
 * Associated object for [[utils.kozak.KzConsensus KzConsensus]], contains implicit type casts.
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
