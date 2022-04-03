package org.vulpinedesigns.initiator_set
package utils.mrna

import utils.kozak.KzContext
import utils.dna.DNABases
import scala.language.implicitConversions

/**
 * An enum for all the base chemicals in RNA;
 *  - A = adenine
 *  - U = uracil
 *  - G = guanine
 *  - C = cytosine
 */
enum MRNABases {
	/**
	 * Converts an [[MRNABases]] to a [[DNABases]] based on a rule such that;
	 *
	 * MRNABases.U -> DNABases.A
	 * MRNABases.G -> DNABases.C
	 * MRNABases.C -> DNABases.G
	 * MRNABases.A -> DNABases.T
	 *
	 * This is a rule purely reflective of what happens to mRNA to DNA bases inside
	 * a cell.
	 *
	 * @return [[DNABases]]: the result of the conversion.
	 */
	def toDNA: DNABases = {
		val mrnaBase = MRNABases.fromOrdinal(ordinal)
		mrnaBase match {
			case MRNABases.U => DNABases.A
			case MRNABases.G => DNABases.C
			case MRNABases.C => DNABases.G
			case MRNABases.A => DNABases.T
		}
	}
	
	case A extends MRNABases
	case U extends MRNABases
	case G extends MRNABases
	case C extends MRNABases
}

/**
 * Associated object for [[mRNA.Bases]], containing extension methods for `List[Bases]`.
 */
object MRNABases {
	/**
	 * A function to iterate over a string of given bases and return an array of the [[MRNABases]] type,
	 * this removes potential unknowns when working with lists of bases.
	 *
	 * @param bases: `String` a string of bases, all characters must be either 'A', 'U', 'G', or 'C'.
	 * @return `Some(List[Bases])` if all the characters are acceptable, `None` if any are out of range.
	 */
	def listFromString(bases: String): Option[List[MRNABases]] = {
		val res = for (
			char <- bases
		) yield char.toUpper match {
			case 'A' => MRNABases.A
			case 'U' => MRNABases.U
			case 'G' => MRNABases.G
			case 'C' => MRNABases.C
			case _ => return None
		}
		Some(res.toList)
	}
	extension (bases: List[MRNABases]) {
		implicit def toString: String = {
			bases.flatMap(b => b.toString).mkString
		}
		def toDNA: List[DNABases] =
			bases.map(_.toDNA)
	}
}

/**
 * Converts an array of 3 [[MRNABases]] (a codon) into a number classifying it.
 * Since there are 4 bases and 3 bases per codon, that works out at 64 possible codons,
 * so a number between 0 and 63.
 *
 * @param codon: `List[Bases]` an array of 3 bases to form a codon.
 * @return `Some(Int)` with the codons value if the param codon is 3 in length, else `None`.
 */
def indexCodon(codon: List[MRNABases]): Option[Int] = {
	if (codon.length != 3) return None
	val res: Int  = codon.reverse.zipWithIndex.map { (base, index) =>
		val multiplier = Math.pow(4, index).toInt
		base.ordinal * multiplier
	}.sum
	Some(res)
}

/**
 * Converts a value between 0-63 into the corresponding array of 3 [[MRNABases]] (a codon).
 * Resistant to negative and out of range numbers.
 *
 * @param codonVal: `Int` a value (0-63) representing one of 64 different possible codons.
 * @return `List[Bases]` a list of 3 bases corresponding to to the passed in `codonVal`.
 */
def deIndexCodon(codonVal: Int): List[MRNABases] = {
	val codonDouble = codonVal.toDouble
	val res = 0 to 2 map { (index: Int) =>
		val divider = Math.pow(4, index)
		MRNABases fromOrdinal (codonDouble / divider).toInt % 4
	}
	res.toList.reverse
}

/**
 * Takes a list of [[MRNABases]], breaks it at each group of three (3 bases being a codon),
 * then generates the index of each codon (a number corresponding to it) into a list.
 * Returns a blank list if `baseList` length is less than 3.
 *
 * @param baseList : `List[Bases]` a list of [[MRNABases]] containing codons to be extracted and indexed.
 * @return `Some(List[Int])` if all the codons were successfully indexed.
 */
def indexCodons(baseList: List[MRNABases]): Option[List[Int]] = {
	val usableLength = baseList.length - 3
	val res = 0 to usableLength map { index =>
		val codon = baseList slice(index, index + 3)
		indexCodon(codon)
	}
	val filteredList = for (
		opt <- res
	) yield opt match {
		case Some(i) => i
		case None => return None
	}
	Some(filteredList.toList)
}

/**
 * Takes a list of numbers representing different combinations of 3 [[MRNABases]] (codons) and
 * converts them into the list of raw bases.
 *
 * @param codonIndexes: `List[Int]` a list of indexes representing codons.
 * @return `List[Bases]` a list of bases expanded from the list of codons.
 */
def deIndexCodons(codonIndexes: List[Int]): List[MRNABases] = {
	val usableLength = codonIndexes.length - 1
	val remaining = usableLength % 3
	val res = 0 to usableLength by 3 map { iter =>
		deIndexCodon(codonIndexes(iter))
	}
	val firstIndexes = res.toList.flatten
	if (remaining != 0) {
		val lastCodon = codonIndexes(usableLength)
		val lastBases = deIndexCodon(lastCodon)
		return firstIndexes ++ lastBases
			.slice(lastBases.length - remaining, lastBases.length)
	}
	firstIndexes
}

/**
 * A class for storing all data to do with an mRNA strand, this is primarily a list of [[MRNABases]],
 * and the corresponding index for all the groups of 3 (codons) within those bases.
 *
 * @param Id: `String` an identifying string to give the mRNA strand.
 * @param Codons: `List[Int]` a list of codons that make up the mRNA strand.
 * @param BaseWeights: `Map[Int, Double]` the assigned 'weight' of all the possible codons.
 * @param AdjustedWeights: `Map[Int, Double]` the adjusted weights of all the possible codons.
 * @param KzContexts: `List[KzContext]` data on any kozak calculations done on the sequence (calculation determining a
 * "start" point).
 */
class mRNA(
	val Id: String,
	val Codons: List[Int],
	val BaseWeights: Map[Int, Double] = Map(),
	val AdjustedWeights: Map[Int, Double] = Map(),
	val KzContexts: List[KzContext] = List()
) {
	val Nucleotide: List[MRNABases] = deIndexCodons(Codons)
}

/**
 * Associated object for [[mRNA]], containing implicit type casts.
 */
object mRNA {
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
