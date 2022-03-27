package org.vulpine_designs.initiator_set.fasta

import org.vulpine_designs.initiator_set.utils.protein.AminoAcid

sealed abstract class AltProteinBases

case class Nothing() extends AltProteinBases
case class Start(aminoAcid: List[AminoAcid]) extends AltProteinBases
case class End(aminoAcid: List[AminoAcid]) extends AltProteinBases

class ProteinMap(
	val Codons: List[Int],
	val ProteinBases: List[AminoAcid],
	val AltAminoAcid: AltProteinBases = Nothing()
) {
	def hasCodon(codon: Int): Boolean = Codons.contains(codon)
	def getAltIfStart: List[AminoAcid] = AltAminoAcid match {
		case Start(altBases) => altBases
		case _ => ProteinBases
	}
	def getAltIfEnd: List[AminoAcid] = AltAminoAcid match {
		case End(altBases) => altBases
		case _ => ProteinBases
	}
}

object ProteinMap {
	extension (list: List[ProteinMap]) {
		def startSequence: List[AminoAcid] = list.headOption match {
			case Some(start) => {
				val res = start.getAltIfStart :: list.tail.map(_.ProteinBases)
				res.flatMap(m => m)
			}
			case _ => List()
		}
		def endSequence: List[AminoAcid] = list.headOption match {
			case Some(start) => {
				val res = start.getAltIfEnd :: list.tail.map(_.ProteinBases)
				res.flatMap(m => m)
			}
			case _ => List()
		}
		def allSequences: List[AminoAcid] = list.flatMap(_.ProteinBases)
	}
}

class ProteinMappings(mappings: List[ProteinMap] = List()) {
	val Mappings: List[ProteinMap] =
		if(mappings.isEmpty) DefaultMappings
		else mappings
	
	def getAminoAcidFromCodon(codon: Int): Option[ProteinMap] =
		Mappings.find(_.hasCodon(codon))
	
	def getAminoAcidsFromCodonList(codons: List[Int]): Option[List[ProteinMap]] = {
		val res = for ( codon <- codons ) yield {
			getAminoAcidFromCodon(codon) match {
				case Some(codon) => codon
				case None => return None
			}
		}
		Some(res)
	}
}

val DefaultMappings: List[ProteinMap] = List(
	ProteinMap(List(45, 47, 44, 46), List(AminoAcid.A)),
	ProteinMap(List(25, 27), List(AminoAcid.C)),
	ProteinMap(List(33, 35), List(AminoAcid.D)),
	ProteinMap(List(32, 34), List(AminoAcid.E)),
	ProteinMap(List(21, 23), List(AminoAcid.F)),
	ProteinMap(List(41, 43, 40, 42), List(AminoAcid.G)),
	ProteinMap(List(49, 51), List(AminoAcid.H)),
	ProteinMap(List(5, 7, 4), List(AminoAcid.I)),
	ProteinMap(List(0, 2), List(AminoAcid.K)),
	ProteinMap(List(20, 22, 53, 55, 52, 54), List(AminoAcid.L)),
	ProteinMap(List(1, 3), List(AminoAcid.N)),
	ProteinMap(List(61, 63, 60, 62), List(AminoAcid.P)),
	ProteinMap(List(48, 50), List(AminoAcid.Q)),
	ProteinMap(List(57, 59, 56, 58, 8, 10), List(AminoAcid.R)),
	ProteinMap(List(29, 31, 28, 30, 9, 11), List(AminoAcid.S)),
	ProteinMap(List(13, 15, 12, 14), List(AminoAcid.T)),
	ProteinMap(List(37, 39, 36, 38), List(AminoAcid.V)),
	ProteinMap(List(26), List(AminoAcid.W)),
	ProteinMap(List(17, 19), List(AminoAcid.Y)),
	ProteinMap(List(6), List(AminoAcid.M), Start(List(AminoAcid.Start, AminoAcid.M))),
	ProteinMap(List(16, 18, 24), List(), End(List(AminoAcid.Stop)))
)
