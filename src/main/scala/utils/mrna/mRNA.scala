package org.vulpinedesigns.initiator_set
package utils.mrna

import utils.kozak.KzContext
import utils.dna.DNABases
import scala.language.implicitConversions


/**
 * A class for storing all data to do with an mRNA strand, this is primarily a list of [[utils.mrna.MRNABases MRNABases]],
 * and the corresponding index for all the groups of 3 (codons) within those bases.
 *
 * @param Id An identifying string to give the mRNA strand.
 * @param Codons A list of codons that make up the mRNA strand.
 * @param BaseWeights The assigned 'weight' of all the possible codons.
 * @param AdjustedWeights The adjusted weights of all the possible codons.
 * @param KzContexts Data on any kozak calculations done on the sequence (calculation determining a
 *                   "start" point).
 */
class mRNA(
	val Id: String,
	val Codons: List[Int],
	val BaseWeights: Map[Int, Double] = Map(),
	val AdjustedWeights: Map[Int, Double] = Map(),
	val KzContexts: List[KzContext] = List()
) {
	/**
	 * The de-indexed list of [[mRNA.Codons]].
	 */
	val Nucleotide: List[MRNABases] = MRNABases.deIndexCodons(Codons)
}


/**
 * Associated object for [[utils.mrna.mRNA mRNA]], containing implicit type casts.
 */
object mRNA {
	
	/**
	 * Overrided method to convert an mRNA class to a string representation.
	 *
	 * @param mrna The input [[utils.mrna.mRNA mRNA]] instance.
	 * @return The string representation.
	 */
	implicit def toString(mrna: mRNA): String = {
		val bases: String = mrna.Nucleotide
		val baseWeights = mrna.BaseWeights.toList
			.flatMap((c, w) => s"$w, ")
			.mkString
			.dropRight(2)
		val adjWeights = mrna.AdjustedWeights.toList
			.flatMap((c, w) => s"$w, ")
			.mkString
			.dropRight(2)
		s"$bases\n[$baseWeights]\n[$adjWeights]"
	}
}
