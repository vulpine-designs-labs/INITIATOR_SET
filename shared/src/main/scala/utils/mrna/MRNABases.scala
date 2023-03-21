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
	 * Converts an [[utils.mrna.MRNABases MRNABases]] to a [[utils.dna.DNABases DNABases]] based on a rule such that;
	 *
	 * MRNABases.U -> DNABases.A
	 * MRNABases.G -> DNABases.C
	 * MRNABases.C -> DNABases.G
	 * MRNABases.A -> DNABases.T
	 *
	 * This is a rule purely reflective of what happens to mRNA to DNA bases inside
	 * a cell.
	 *
	 * @return The result of the conversion.
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
	
	/** A = adenine */
	case A extends MRNABases
	/** U = uracil */
	case U extends MRNABases
	/** G = guanine */
	case G extends MRNABases
	/** C = cytosine */
	case C extends MRNABases
}


/**
 * Associated object for [[utils.mrna.MRNABases MRNABases]], containing extension methods for `List[Bases]`
 * and associated functions.
 */
object MRNABases {
	/**
	 * A function to iterate over a string of given bases and return an array of the [[utils.mrna.MRNABases MRNABases]] type,
	 * this removes potential unknowns when working with lists of bases.
	 *
	 * @param bases A string of bases, all characters must be either 'A', 'U', 'G', or 'C'.
	 * @return If all the characters are acceptable, `None` if any are out of range.
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
	
	/**
	 * Converts an array of 3 [[utils.mrna.MRNABases MRNABases]] (a codon) into a number classifying it.
	 * Since there are 4 bases and 3 bases per codon, that works out at 64 possible codons,
	 * so a number between 0 and 63.
	 *
	 * @param codon An array of 3 bases to form a codon.
	 * @return With the codons value if the param codon is 3 in length, else `None`.
	 */
	def indexCodon(codon: List[MRNABases]): Option[Int] = {
		if (codon.length != 3) return None
		val res: Int  = codon.reverse.zipWithIndex.map(getValue).sum
		Some(res)
	}
	
	private def getValue(base: MRNABases, index: Int): Int = {
		val multiplier = Math.pow(4, index).toInt
		val res = base.ordinal * multiplier
		res
	}
	
	/**
	 * Converts a value between 0-63 into the corresponding array of 3 [[utils.mrna.MRNABases MRNABases]] (a codon).
	 * Resistant to negative and out of range numbers.
	 *
	 * @param codonVal A value (0-63) representing one of 64 different possible codons.
	 * @return A list of 3 bases corresponding to to the passed in `codonVal`.
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
	 * Takes a list of [[utils.mrna.MRNABases MRNABases]], breaks it at each group of three (3 bases being a codon),
	 * then generates the index of each codon (a number corresponding to it) into a list.
	 * Returns a blank list if `baseList` length is less than 3.
	 *
	 * @param baseList A list of [[utils.mrna.MRNABases MRNABases]] containing codons to be extracted and indexed.
	 * @return If all the codons were successfully indexed.
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
	 * Takes a list of numbers representing different combinations of 3 [[utils.mrna.MRNABases MRNABases]] (codons) and
	 * converts them into the list of raw bases.
	 *
	 * @param codonIndexes A list of indexes representing codons.
	 * @return A list of bases expanded from the list of codons.
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
	
	extension (bases: List[MRNABases]) {
		/**
		 * An extension method that will iterate over a list of mRNA bases and convert them to
		 * a string based on [[utils.mrna.MRNABases.toString MRNABases.toString]], this can be reversed by
		 * feeding the output into [[utils.mrna.MRNABases.listFromString() MRNABases.listFromString]].
		 *
		 * @return The output string.
		 */
		implicit def toString: String = {
			bases.flatMap(b => b.toString).mkString
		}
		
		/**
		 * An extension method that will iterate over a list of mRNA bases and convert them to
		 * [[utils.dna.DNABases DNABases]] based on [[utils.mrna.MRNABases.toDNA MRNABases.toDNA]].
		 *
		 * @return The converted list.
		 */
		def toDNA: List[DNABases] =
			bases.map(_.toDNA)
	}
}
