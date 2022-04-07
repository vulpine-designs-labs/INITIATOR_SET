package org.vulpinedesigns.initiator_set

import utils.kozak.KzContext
import utils.mrna.{
	mRNA,
	MRNABases
}


/**
 * Contains methods for calculating the "leakiness" of a start codon.
 *
 * That is, when a codon in a chain of codons (a codon being Int of 0-63 representing possible
 * combinations of 3 [[utils.mrna.MRNABases MRNABases]]) is identified as a start codon (the
 * [[utils.kozak.KzContext.Strength KzContext.Strength]] is particularly strong).
 *
 * It is defined as leaky if the given start codon has a weak [[utils.kozak.KzContext.Strength KzContext.Strength]]
 * or a weak value defined in [[utils.mrna.mRNA.AdjustedWeights mRNA.AdjustedWeights]].
 */
object leaky_scanning {
	/**
	 * Calculates the averages for each [[utils.mrna.mRNA.AdjustedWeights mRNA.AdjustedWeights]] and
	 * [[utils.kozak.KzContext.Strength KzContext.Strength]] in [[utils.mrna.mRNA.KzContexts mRNA.KzContexts]],
	 * and can also apply a  list of penalties with on each item, passing in the
	 * [[utils.kozak.KzContext.ContextStart KzContext.ContextStart]] for each item.
	 *
	 * A result of 0 means the [[utils.kozak.KzContext KzContext]] has a been compared such that it matches and nothing
	 * has been returned from the penalties.
	 *
	 * @param mrna The mRNA containing the weights [[utils.mrna.mRNA.BaseWeights mRNA.BaseWeights]] to be adjusted.
	 * @param penalties A list of functions that can apply a penalty based on
	 *                  [[utils.kozak.KzContext.ContextStart KzContext.ContextStart]].
	 * @return The "leakiness" score for each position, returns `None` if there is fewer
	 *         [[utils.mrna.mRNA.AdjustedWeights mRNA.AdjustedWeights]] than [[utils.mrna.mRNA.KzContexts mRNA.KzContexts]].
	 */
	def calculateLeakyChronological(mrna: mRNA, penalties: List[Int => Double]): Option[List[Double]] = {
		val kzContexts = mrna.KzContexts
		val adjustedWeights = mrna.AdjustedWeights
		val res = for (
			(context, index) <- kzContexts.zipWithIndex
		) yield {
			val weight = if(adjustedWeights.contains(index)) adjustedWeights(index)
			else return None
			makeLeakyValue(context, weight, penalties)
		}
		Some(res)
	}
	
	private def makeLeakyValue(context: KzContext, relativeStrength: Double, penalties: List[Int => Double]): Double = {
		val penaltiesLength = 2 + penalties.length
		val strength = context.Strength
		val pos = context.InitiatorStart
		val combinedPenalties = penalties.map(_(pos)).sum
		1 - (relativeStrength + strength + combinedPenalties) / penaltiesLength
	}
	
	/**
	 * The default stop codons used by the module.
	 *
	 * [[utils.mrna.MRNABases.U MRNABases.U]], [[utils.mrna.MRNABases.A MRNABases.A]], [[utils.mrna.MRNABases.G MRNABases.G]]
	 *
	 * [[utils.mrna.MRNABases.U MRNABases.U]], [[utils.mrna.MRNABases.U MRNABases.U]], [[utils.mrna.MRNABases.A MRNABases.A]]
	 *
	 * [[utils.mrna.MRNABases.U MRNABases.U]], [[utils.mrna.MRNABases.G MRNABases.G]], [[utils.mrna.MRNABases.A MRNABases.A]]
	 *
	 */
	val DefaultStopCodons: List[Int] = List(
		List(MRNABases.U, MRNABases.A, MRNABases.G),
		List(MRNABases.U, MRNABases.U, MRNABases.A),
		List(MRNABases.U, MRNABases.G, MRNABases.A)
	).map(codon => MRNABases.indexCodon(codon).getOrElse(0))
	
	/**
	 * Loops over [[utils.mrna.mRNA.Codons mRNA.Codons]], calculating the sequence lengths
	 * (via [[sequenceLengths]]) at each index in the sequence).
	 *
	 * @param mrna The [[utils.mrna.mRNA mRNA]] with the sequence.
	 * @return The calculated lengths.
	 */
	def calculateLengths(mrna: mRNA): List[Int] = {
		mrna.Codons.zipWithIndex map { (codon, index) =>
			sequenceLengths(mrna, index)
		}
	}
	
	/**
	 * Takes an [[utils.mrna.mRNA mRNA]] and iterates over [[utils.mrna.mRNA.Codons mRNA.Codons]] from the position indicated by
	 * `start`, returning the first index where a value is within `stopCodons`, thus getting
	 * the apparent length of the sequence.
	 *
	 * @param mrna The mrna with the sequence to be checked.
	 * @param start The index of the sequence to be checked from.
	 * @param stopCodons A list of codons designated as stop codons, defaults to [[DefaultStopCodons]].
	 * @return The first position containing a stop codon.
	 */
	def sequenceLengths(mrna: mRNA, start: Int, stopCodons: List[Int] = DefaultStopCodons): Int = {
		val codons = mrna.Codons.slice(start, mrna.Codons.length)
		val potentialStops = stopCodons.map(codons.indexOf(_)).filter(_ != -1)
		potentialStops.min
	}
	
	/**
	 * Determines whether from a list of sequence lengths (e.g. calculated by
	 * [[leaky_scanning.calculateLengths() calculateLengths]])
	 * 2 sequences with starting positions indicated by `one: Int` and `two: Two` overlap.
	 *
	 * @param sequence The list of sequence lengths.
	 * @param one The starting position of the first sequence.
	 * @param two The start position of the other sequence.
	 * @return Whether or not they overlap, returns `None` if the lowest starting
	 *         position os out of bounds of `sequence`.
	 */
	def doSequencesOverlap(sequence: List[Int], one: Int, two: Int): Option[Boolean] = {
		val seqStarts = List(one, two)
		val start = seqStarts.min
		val end = seqStarts.max
		if(sequence.length <= start) None
		else Some(sequence(start) + start > end)
	}
}
