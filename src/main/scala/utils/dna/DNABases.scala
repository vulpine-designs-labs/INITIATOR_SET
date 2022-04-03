package org.vulpine_designs.initiator_set
package utils.dna

import utils.protein.AminoAcid
import utils.mrna.MRNABases
import scala.language.implicitConversions

/**
 * An enum for all the base chemicals in RNA;
 *  - A = adenine
 *  - U = uracil
 *  - G = guanine
 *  - C = cytosine
 */
enum DNABases {
	/**
	 * Converts an [[DNABases]] to a [[MRNABases]] based on a rule such that;
	 *
	 * DNABases.A -> MRNABases.U
	 * DNABases.C -> MRNABases.G
	 * DNABases.G -> MRNABases.C
	 * DNABases.T -> MRNABases.A
	 *
	 * This is a rule purely reflective of what happens to DNA to MRNA bases inside
	 * a cell.
	 *
	 * @return [[MRNABases]]: the result of the conversion.
	 */
	def toMRNA: MRNABases = {
		val dnaBase = DNABases.fromOrdinal(ordinal)
		dnaBase match {
			case DNABases.A => MRNABases.U
			case DNABases.C => MRNABases.G
			case DNABases.G => MRNABases.C
			case DNABases.T => MRNABases.A
		}
	}
	
	case A extends DNABases
	case C extends DNABases
	case G extends DNABases
	case T extends DNABases
}

object DNABases {
	/**
	 * A function to iterate over a string of given bases and return an array of the [[DNABases]] type,
	 * this removes potential unknowns when working with lists of bases.
	 *
	 * @param string: `String` a string of bases, all characters must be either 'A', 'C', 'G', or 'T'.
	 * @return `Some(List[DNABases])` if all the characters are acceptable, `None` if any are out of range.
	 */
	def listFromString(string: String): Option[List[DNABases]] = {
		val res = for ( char <- string ) yield {
			try {
				DNABases.valueOf(char.toString)
			} catch {
				case e: Exception => return None
			}
		}
		Some(res.toList)
	}
	
	extension (bases: List[DNABases]) {
		implicit def toString: String = {
			bases.flatMap(_.toString).mkString
		}
		
		/**
		 * Iterates over the bases and calls [[DNABases.toMRNA]] on each to get the corresponding
		 * mRNA base for each.
		 *
		 * @return `List[MRNABases]`: the list of all the corresponding [[MRNABases]]
		 */
		def toMRNA: List[MRNABases] =
			bases.map(_.toMRNA)
	}
}