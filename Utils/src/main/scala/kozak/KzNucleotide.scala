package org.vulpine_designs.initiator_set.utils
package kozak

import mRNA._
import scala.language.implicitConversions

/**
 * The minimum value that will make a base dominant in [[KzNucleotide.NucleotideDict]].
 */
val DominantMin: Double = 0.4

/**
 * This represents a position within the Kozak sequence.
 *
 * Different positions within a kozak sequence can have greater bearing on the "strength"
 * of sequence, furthermore different bases at certain positions can also increase
 * the strength of a sequence.
 *
 * This contains the "strength" or "weight" of each base at that position as well as the overall
 * importance of that position.
 *
 * @param aWeight: `Double` the weight of [[mRNA.Bases.A]] at this position.
 * @param uWeight: `Double` the weight of [[mRNA.Bases.U]] at this position.
 * @param gWeight: `Double` the weight of [[mRNA.Bases.G]] at this position.
 * @param cWeight: `Double` the weight of [[mRNA.Bases.C]] at this position.
 * @param Importance: `Int` the importance of this position.
 */
class KzNucleotide (
		aWeight: Double,
		uWeight: Double,
		gWeight: Double,
		cWeight: Double,
		val Importance: Int
) {
	val NucleotideDict = Map(
		MRNABases.A -> aWeight,
		MRNABases.U -> uWeight,
		MRNABases.G -> gWeight,
		MRNABases.C -> cWeight
	)
	
	/**
	 * A function to return a list of [[mRNA.Bases]] along with the weight of them (in a Tuple)
	 * at the current position in descending order.
	 *
	 * @return `List[(Bases, Double)]` the bases along with the weights of those bases.
	 */
	def sortDescending: List[(MRNABases, Double)] =
		NucleotideDict.toList.sortBy((base, value) => value).reverse
	
	/**
	 * Returns a list of the [[mRNA.Bases]] with the highest weight, allowing for joint highest.
	 *
	 * @return `List[(Bases, Double)]` the bases along with the weights of those bases.
	 */
	def maximumNucleotideDist: List[(MRNABases, Double)] = {
		val (_base, topDist) = sortDescending.head
		NucleotideDict.toList.filter((base, value) => value == topDist)
	}
	
	/**
	 * Returns a list of [[mRNA.Bases]] along with the weights for them, filtered out
	 * to only contain ones with weight surpassing the minimum dominant weight,
	 * defined in [[DominantMin]].
	 *
	 * @return `List[(Bases, Double)]` the bases along with the weights of those bases.
	 */
	def dominantNucleotideDist: List[(MRNABases, Double)] = {
		val (_base, topDist) = sortDescending.head
		NucleotideDict.toList.filter((base, value) =>
			value == topDist || value > DominantMin
		)
	}
	
	/**
	 * Returns a list of [[mRNA.Bases]] without the weights, filtered out
	 * to only contain ones with weight surpassing the minimum dominant weight,
	 * defined in [[DominantMin]].
	 *
	 * @return `List[Bases]` the filtered bases.
	 */
	def dominantNucleotides: List[MRNABases] = {
		val (_base, topDist) = sortDescending.head
		val filteredDist: List[(MRNABases, Double)] = NucleotideDict.toList.filter((base, value) =>
			value == topDist || value > DominantMin
		)
		filteredDist.map((base, value) => base)
	}
	
	/**
	 * Returns the "confidence" of a given base at the current position.
	 *
	 * A base with a high weight at an important position means there is a greater "confidence"
	 * that the base is within the desired sequence looking for by the kozak. See module
	 * definition for [[kozak]] for definition on kozak sequences.
	 *
	 * @param base: [[mRNA.Bases]] the base that needs checking.
	 *
	 * @return `Double` the "confidence" value.
	 */
	def getConfidence(base: MRNABases): Double = {
		val weight = NucleotideDict.getOrElse(base, 0.0)
		if(Importance < 0 && !dominantNucleotides.contains(base))
			-1
		else if(Importance >= 0)
			weight * Importance.toDouble
		else
			1.0
	}
}

/**
 * Associated object for [[KzNucleotide]], containing implicit type casts. 
 */
object KzNucleotide {
	implicit def toString(kozakNuc: KzNucleotide): String = {
		val dominants = kozakNuc.dominantNucleotideDist
		if(dominants.length <= 0) {
			return s"${dominants.head._1}"
		}
		val string: String = dominants.map((base, _value) => s"$base")
			.mkString("|")
		s"[$string]"
	}
}


