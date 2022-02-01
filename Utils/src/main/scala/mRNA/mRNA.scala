package org.vulpine_designs.initiator_set.utils
/**
 * Contains helper types and functions for handling and representing mRNA.
 */
package mRNA

/**
 * An enum for all the base chemicals in RNA;
 *  - A = adenine
 *  - U = uracil
 *  - G = guanine
 *  - C = cytosine
 */
enum Bases {
	case A extends Bases
	case U extends Bases
	case G extends Bases
	case C extends Bases
}

/**
 * A function to iterate over a string of given bases and return an array of the [[Base]] type,
 * this removes potential unknowns when working with lists of bases.
 *
 * @param bases: `String` a string of bases, all characters must be either 'A', 'U', 'G', or 'C'.
 * @return `Some(List[Bases])` if all the characters are acceptable, `None` if any are out of range.
 */
def basesFromString(bases: String): Option[List[Bases]] = {
	val r = for (
		char <- bases
	) yield char.toUpper match {
		case 'A' => Bases.A
		case 'U' => Bases.U
		case 'G' => Bases.G
		case 'C' => Bases.C
		case _ => return None
	}
	val x = Bases.valueOf("A")
	Some(r.toList)
}

/**
 * Converts an array of 3 [[Bases]] (a codon) into a number classifying it.
 * Since there are 4 bases and 3 bases per codon, that works out at 64 possible codons,
 * so a number between 0 and 63.
 *
 * @param codon: `List[Bases]` an array of 3 bases to form a codon.
 *
 * @return `Some(Int)`` with the codons value if the param codon` is 3 in length, else `None`.
 */
def indexCodon(codon: List[Bases]): Option[Int] = {
	if (codon.length != 3) return None
	val res  = codon.zipWithIndex.map { (base, index) =>
		val multiplier = 4 ^ index
		base.ordinal * multiplier
	}.sum
	Some(res)
}

/**
 * Converts a value between 0-63 into the corresponding array of 3 [[Bases]] (a codon).
 * Resistant to negative and out of range numbers.
 *
 * @param codonVal: `Int` a value (0-63) representing one of 64 different possible codons.
 *
 * @return `List[Bases]` a list of 3 bases corresponding to to the passed in `codonVal`.
 */
def deIndexCodon(codonVal: Int): List[Bases] = {
	val codonDouble = codonVal.toDouble
	val res = 0 to 2 map { (index: Int) =>
		val divider = (4 ^ index).toDouble
		Bases fromOrdinal (codonDouble / divider).toInt % 4
	}
	res.toList
}

/**
 * Takes a list of [[Bases]], breaks it at each group of three (3 bases being a codon),
 * then generates the index of each codon (a number corresponding to it) into a list.
 * Returns a blank list if `baseList` length is less than 3.
 *
 * @param baseList: `List[Bases]` a list of [[Bases]] containing codons to be extracted and indexed.
 *
 * @return `Some(List[Int])` if all the codons were successfully indexed.
 */
def makeCodonIndexes(baseList: List[Bases]): Option[List[Int]] = {
	val usableLength = baseList.length - 3
	val res = 0 to usableLength map { index =>
		val codon = baseList slice (index, index + 3)
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
 * Takes a list of numbers representing different combinations of 3 [[Bases]] (codons) and
 * converts them into the list of raw bases.
 *
 * @param codonIndexes: `List[Int]` a list of indexes representing codons.
 *
 * @return `codonIndexes: List[Bases]` a list of bases expanded from the list of codons.
 */
def deIndexCodonIndexes(codonIndexes: List[Int]): List[Bases] = {
	val usableLength = codonIndexes.length - 1
	val remaining = usableLength % 3
	val res = 0 to usableLength by 3 map { iter =>
		deIndexCodon(iter)
	}
	val firstIndexes = res.toList.flatten
	if (remaining != 0) {
		val lastCodon = codonIndexes(usableLength)
		return firstIndexes ++ deIndexCodon(lastCodon)
			.slice(usableLength - remaining, codonIndexes.length)
	}
	firstIndexes
}

/**
 * A class for storing all data to do with an mRNA strand, this is primarily a list of [[Bases]],
 * and the corresponding index for all the groups of 3 (codons) within those bases.
 *
 * @param id: `String` an identifying string to give the mRNA strand.
 * @param nucleotide: `List[Bases]` a list of bases that make up the mRNA strand
 */
class mRNA(val id: String, val nucleotide: List[Bases]) {

}
