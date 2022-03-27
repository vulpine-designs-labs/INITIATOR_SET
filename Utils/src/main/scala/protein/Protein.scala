package org.vulpine_designs.initiator_set.utils
package protein

import scala.language.implicitConversions

enum AminoAcid {
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

object AminoAcid {
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
		implicit def toString: String = {
			list.flatMap(_.toString).mkString
		}
	}
}
