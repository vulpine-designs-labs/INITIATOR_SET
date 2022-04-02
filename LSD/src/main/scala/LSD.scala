package org.vulpine_designs.initiator_set.lsd

import org.vulpine_designs.initiator_set.utils.mRNA.{
	mRNA,
	MRNABases,
	indexCodon
}
import org.vulpine_designs.initiator_set.utils.kozak.KzContext

/**
 * The default stop codons used by the module.
 *
 * [[MRNABases.U]], [[MRNABases.A]], [[MRNABases.G]]
 * [[MRNABases.U]], [[MRNABases.U]], [[MRNABases.A]]
 * [[MRNABases.U]], [[MRNABases.G]], [[MRNABases.A]]
 */
val DefaultStopCodons: List[Int] = List(
	List(MRNABases.U, MRNABases.A, MRNABases.G),
	List(MRNABases.U, MRNABases.U, MRNABases.A),
	List(MRNABases.U, MRNABases.G, MRNABases.A)
).map(codon => indexCodon(codon).getOrElse(0))

/**
 * Calculates the averages for each [[mRNA.AdjustedWeights]] and [[KzContext.Strength]] in
 * [[mRNA.KzContexts]], and can also apply a list of penalties with on each item, passing in
 * the [[KzContext.ContextStart]] for each item.
 *
 * A result of 0 means the [[KzContext]] has a been compared such that it matches and nothing
 * has been returned from the penalties.
 *
 * @param mrna `[[mRNA]]`: the mRNA containing the weights [[mRNA.BaseWeights]] to be adjusted
 * @param penalties `List[Int => Double]`: a list of functions that can apply a penalty based on
 *                  KzContext.ContextStart]
 * @return `Option[List[Double]]`: the "leakiness" score for each position, returns `None` if
 *         there is fewer [[mRNA.AdjustedWeights]] than [[mRNA.KzContexts]]
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
 * Loops over [[mRNA.Codons]], calculating the sequence lengths (via [[sequenceLengths]]) at each
 * index in the sequence.
 *
 * @param mrna `[[mRNA]]`: the [[mRNA]] with the sequence
 * @return `List[Int]`: the calculated lengths
 */
def calculateLengths(mrna: mRNA): List[Int] = {
	mrna.Codons.zipWithIndex map { (codon, index) =>
		sequenceLengths(mrna, index)
	}
}

/**
 * Takes an [[mRNA]] and iterates over [[mRNA.Codons]] from the posistion indicated by
 * `start`, returning the first index where a value is within [[stopCodons]], thus getting
 * the apparent length of the sequence.
 *
 * @param mrna `[[mRNA]]`: the mrna with the sequence to be checked
 * @param start ``: the index of the sequence to be checked from
 * @param stopCodons ``: a list of codons designated as stop codons, defaults to [[DefaultStopCodons]]
 * @return `Int`: the first position containing a stop codon
 */
def sequenceLengths(mrna: mRNA, start: Int, stopCodons: List[Int] = DefaultStopCodons): Int = {
	val codons = mrna.Codons.slice(start, mrna.Codons.length)
	val potentialStops = stopCodons.map(codons.indexOf(_)).filter(_ != -1)
	potentialStops.min
}

/**
 * Determines whether from a list of sequence lengths (e.g. calculated by [[calculateLengths]])
 * 2 sequences with starting positions indicated by `one: Int` and `two: Two` overlap.
 *
 * @param sequence `List[Int]`: the list of sequence lengths
 * @param one `Int`: the starting position of the first sequence
 * @param two `Int`: the start position of the other sequence
 * @return `Option[Boolean]`: whether or not they overlap, returns `None` if the
 *         lowest starting position os out of bounds of `sequence`
 */
def doSequencesOverlap(sequence: List[Int], one: Int, two: Int): Option[Boolean] = {
	val seqStarts = List(one, two)
	val start = seqStarts.min
	val end = seqStarts.max
	if(sequence.length <= start) None
	else Some(sequence(start) + start > end)
}
