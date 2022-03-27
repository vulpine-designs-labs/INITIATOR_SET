package org.vulpine_designs.initiator_set.utils
package DNA

import mRNA.MRNABases
import protein.AminoAcid
import scala.language.implicitConversions

enum DNABases {
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
		def toMRNA: List[MRNABases] =
			bases.map(_.toMRNA)
	}
}
