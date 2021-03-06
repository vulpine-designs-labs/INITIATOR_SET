package org.vulpinedesigns.initiator_set
package utils.protein

import scala.language.implicitConversions


/**
 * An enum representing all the types of amino acids which in a list make up a
 * protein. This includes [[utils.protein.AminoAcid.Gap AminoAcid.Gap]] for any gaps in the protein chain,
 * and [[utils.protein.AminoAcid.Stop AminoAcid.Stop]] + [[utils.protein.AminoAcid.Start AminoAcid.Start]]
 * indicating stops and starts of a protein chain.
 */
enum AminoAcid {
	/**
	 * Overrided method for converting an amino acid to a standard string representation.
	 *
	 * @return The string representation.
	 */
	override def toString: String = {
		val value = AminoAcid.fromOrdinal(ordinal)
		value match {
			case AminoAcid.Stop => "*"
			case AminoAcid.Gap => "-"
			case _ => super.toString
		}
	}
	
	case A extends AminoAcid
	case B extends AminoAcid
	case C extends AminoAcid
	case D extends AminoAcid
	case E extends AminoAcid
	case F extends AminoAcid
	case G extends AminoAcid
	case H extends AminoAcid
	case I extends AminoAcid
	case J extends AminoAcid
	case K extends AminoAcid
	case L extends AminoAcid
	case M extends AminoAcid
	case N extends AminoAcid
	case O extends AminoAcid
	case P extends AminoAcid
	case Q extends AminoAcid
	case R extends AminoAcid
	case S extends AminoAcid
	case T extends AminoAcid
	case U extends AminoAcid
	case V extends AminoAcid
	case W extends AminoAcid
	case Y extends AminoAcid
	case Z extends AminoAcid
	case X extends AminoAcid
	case Start extends AminoAcid
	case Stop extends AminoAcid
	case Gap extends AminoAcid
}

/**
 * Associated object for [[utils.protein.AminoAcid AminoAcid]] containing associated functions and overloaded
 * operators and conversions.
 */
object AminoAcid {
	/**
	 * Takes a string, loops through each character and returns the equivalent amino acid
	 * in a list, returning a `None` if there is no match.
	 *
	 * @param string The string to be converted.
	 * @return The result of the attempted conversion.
	 */
	def listFromString(string: String): Option[List[AminoAcid]] = {
		val res = for ( char <- string ) yield {
			try {
				AminoAcid.valueOf(char.toString)
			} catch {
				case e: Exception => return None
			}
		}
		Some(res.toList)
	}
	
	extension (list: List[AminoAcid]) {
		
		/**
		 * An extension method that will iterate over a list of AminoAcids and convert them to
		 * a string based on [[utils.protein.AminoAcid.toString AminoAcid.toString]], this can be reversed by
		 * feeding the output into [[utils.protein.AminoAcid.listFromString() AminoAcid.listFromString]].
		 *
		 * @return The output string.
		 */
		implicit def toString: String = {
			list.flatMap(_.toString).mkString
		}
	}
}

