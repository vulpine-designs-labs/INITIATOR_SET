package org.vulpine_designs.initiator_set.lsd

import org.vulpine_designs.initiator_set.utils.mRNA.{
	mRNA,
	MRNABases,
	indexCodon
}
import org.vulpine_designs.initiator_set.utils.kozak.
	KzContext

val DefaultStopCodons: List[Int] = List(
	List(MRNABases.U, MRNABases.A, MRNABases.G),
	List(MRNABases.U, MRNABases.U, MRNABases.A),
	List(MRNABases.U, MRNABases.G, MRNABases.A)
).map(codon => indexCodon(codon).getOrElse(0))

def calculateLeakyChronological(mrna: mRNA, penalties: List[Int => Double]): Option[List[Double]] = {
	val kzContexts = mrna.KzContexts
	val adjustedWeights = mrna.AdjustedWeights
	val res = for (
		(context, index) <- kzContexts.zipWithIndex
	) yield {
		val weight = if(adjustedWeights.contains(index)) adjustedWeights(index)
			else return None
		getLeaky(context, weight, penalties)
	}
	Some(res)
}

private def getLeaky(context: KzContext, weight: Double, penalties: List[Int => Double]): Double = {
	val penaltiesLength = if (2 * penalties.length == 0) 1
		else 2 * penalties.length
	val pos = context.InitiatorStart
	val strength = context.Strength
	val relativeStrength = weight
	val combinedPenalties = penalties.map(_(pos)).sum
	1 - (relativeStrength + strength + combinedPenalties) / penaltiesLength
}

def calculateLengths(mrna: mRNA): List[Int] = {
	mrna.Codons.zipWithIndex map { (codon, index) =>
		sequenceLengths(mrna, index)
	}
}

def sequenceLengths(mrna: mRNA, start: Int, stopCodons: List[Int] = DefaultStopCodons): Int = {
	val codons = mrna.Codons.slice(start, mrna.Codons.length)
	val potentialStops = stopCodons.map(codons.indexOf(_)).filter(_ != -1)
	potentialStops.min
}

def doSequencesOverlap(sequence: List[Int], one: Int, two: Int): Option[Boolean] = {
	val seqStarts = List(one, two)
	val start = seqStarts.min
	val end = seqStarts.max
	if(sequence.length <= start) None
	else Some(sequence(start) + start > end)
}

def isLeaky(leakyWeights: List[Double], threshold: Double): Boolean = 
	leakyWeights.applyOrElse(0, _ => 0.0) > threshold
