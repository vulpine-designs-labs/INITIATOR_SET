package org.vulpinedesigns.initiator_set
package utils.kozak

import utils.mrna.MRNABases
import scala.language.implicitConversions

/**
 * The minimum value that will make a base dominant in [[utils.kozak.KzNucleotide.NucleotideDict KzNucleotide.NucleotideDict]].
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
 * @param aWeight The weight of [[utils.mrna.MRNABases.A MRNABases.A]] at this position.
 * @param uWeight The weight of [[utils.mrna.MRNABases.U MRNABases.U]] at this position.
 * @param gWeight The weight of [[utils.mrna.MRNABases.G MRNABases.G]] at this position.
 * @param cWeight The weight of [[utils.mrna.MRNABases.C MRNABases.C]] at this position.
 * @param Importance The importance of this position.
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
	 * A function to return a list of [[utils.mrna.MRNABases MRNABases]] along with the weight of them (in a Tuple)
	 * at the current position in descending order.
	 *
	 * @return The bases along with the weights of those bases.
	 */
	def sortDescending: List[(MRNABases, Double)] =
		NucleotideDict.toList.sortBy((base, value) => value).reverse
	
	/**
	 * Returns a list of the [[utils.mrna.MRNABases MRNABases]] with the highest weight, allowing for joint highest.
	 *
	 * @return The bases along with the weights of those bases.
	 */
	def maximumNucleotideDist: List[(MRNABases, Double)] = {
		val (_base, topDist) = sortDescending.head
		NucleotideDict.toList.filter((base, value) => value == topDist)
	}
	
	/**
	 * Returns a list of [[utils.mrna.MRNABases MRNABases]] along with the weights for them, filtered out
	 * to only contain ones with weight surpassing the minimum dominant weight,
	 * defined in [[utils.kozak.DominantMin DominantMin]].
	 *
	 * @return The bases along with the weights of those bases.
	 */
	def dominantNucleotideDist: List[(MRNABases, Double)] = {
		val (_base, topDist) = sortDescending.head
		NucleotideDict.toList.filter((base, value) =>
			value == topDist || value > DominantMin
		)
	}
	
	/**
	 * Returns a list of [[utils.mrna.MRNABases MRNABases]] without the weights, filtered out
	 * to only contain ones with weight surpassing the minimum dominant weight,
	 * defined in [[utils.kozak.DominantMin DominantMin]].
	 *
	 * @return The filtered bases.
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
	 * that the base is within the desired sequence looking for by the kozak.
	 *
	 * @param base The base that needs checking.
	 * @return The "confidence" value.
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
 * Associated object for [[utils.kozak.KzNucleotide KzNucleotide]], containing implicit type casts. 
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
