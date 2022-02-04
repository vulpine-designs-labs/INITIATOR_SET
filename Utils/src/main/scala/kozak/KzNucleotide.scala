package org.vulpine_designs.initiator_set.utils

/**
 * Contains helper types and functions for handling and representing Kozak contexts.
 *
 * Kozaks are a specific sequence identified within a sequence of [[mRNA.Bases]] that
 * denote where the sequence should be read from, these sequences can be stronger or weaker.
 */
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
 * This contains the "weight" of each base at that position as well as the overall
 * importance of that position.
 *
 * @param aWeight: `Double` the weight of [[Bases.A]] at this position.
 * @param uWeight: `Double` the weight of [[Bases.U]] at this position.
 * @param gWeight: `Double` the weight of [[Bases.G]] at this position.
 * @param cWeight: `Double` the weight of [[Bases.C]] at this position.
 * @param Importance: `Int` the importance of this position.
 */
class KzNucleotide(
		aWeight: Double,
		uWeight: Double,
		gWeight: Double,
		cWeight: Double,
		val Importance: Int
) {
	val NucleotideDict = Map(
		Bases.A -> aWeight,
		Bases.U -> uWeight,
		Bases.G -> gWeight,
		Bases.C -> cWeight
	)
	
	/**
	 * A function to return a list of [[Bases]] along with the weight of them (in a Tuple)
	 * at the current position in descending order.
	 *
	 * @return `List[(Bases, Double)]` the bases along with the weights of those bases.
	 */
	def sortDescending: List[(Bases, Double)] =
		NucleotideDict.toList.sortBy((base, value) => value).reverse
	
	/**
	 * Returns a list of the [[Bases]] with the highest weight, allowing for joint highest.
	 *
	 * @return `List[(Bases, Double)]` the bases along with the weights of those bases.
	 */
	def maximumNucleotideDist: List[(Bases, Double)] = {
		val (_base, topDist) = sortDescending.head
		NucleotideDict.toList.filter((base, value) => value == topDist)
	}
	
	/**
	 * Returns a list of [[Bases]] along with the weights for them, filtered out
	 * to only contain ones with weight surpassing the minimum dominant weight,
	 * defined in [[DominantMin]].
	 *
	 * @return `List[(Bases, Double)]` the bases along with the weights of those bases.
	 */
	def dominantNucleotideDist: List[(Bases, Double)] = {
		val (_base, topDist) = sortDescending.head
		NucleotideDict.toList.filter((base, value) =>
			value == topDist || value > DominantMin
		)
	}
}

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


