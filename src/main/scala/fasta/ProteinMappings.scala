package org.vulpinedesigns.initiator_set
package fasta

import utils.protein.AminoAcid
import scala.language.implicitConversions


/**
 * A base class for if a different list of amino acids is required if a certain mRNA codon is
 * at the start or end of a [[utils.mrna.MRNABases MRNABases]] sequence.
 */
sealed abstract class AltProteinBases

/**
 * A case class for if there are no alternative amino acids if a codon is at the start or end.
 */
case class Nothing() extends AltProteinBases

/**
 * A case class for if a codon is at the start of a sequence and converts to a different
 * set of AminoAcids.
 *
 * @param aminoAcid The alternative list of amino acids.
 */
case class Start(aminoAcid: List[AminoAcid]) extends AltProteinBases

/**
 * A case class for if a codon is at the end of a sequence and converts to a different
 * set of AminoAcids.
 *
 * @param aminoAcid The alternative list of amino acids.
 */
case class End(aminoAcid: List[AminoAcid]) extends AltProteinBases


/**
 * A class for associating a list of mRNA codons to a list of amino acids (a protein).
 *
 * @param Codons The list of codons that convert to the given amino acids.
 * @param ProteinBases The list of AminoAcids that match to the codons.
 * @param AltAminoAcid Potential alternative set of amino acids if the codon is at
 *                     the start or end.
 */
class ProteinMap(
	val Codons: List[Int],
	val ProteinBases: List[AminoAcid],
	val AltAminoAcid: AltProteinBases = Nothing()
) {
	/**
	 * Checks if the current map contains a given codon value.
	 *
	 * @param codon The given codon to check.
	 * @return Whether or not the map has the codon.
	 */
	def hasCodon(codon: Int): Boolean = Codons.contains(codon)
	
	/**
	 * Gets the alt list of amino acids if there are any alts for start, else
	 * returns the main list.
	 *
	 * @return The list of amino acids in the map.
	 */
	def getAltIfStart: List[AminoAcid] = AltAminoAcid match {
		case Start(altBases) => altBases
		case _ => ProteinBases
	}
	
	/**
	 * Gets the alt list of amino acids if there are any alts for end, else
	 * returns the main list.
	 *
	 * @return The list of amino acids in the map.
	 */
	def getAltIfEnd: List[AminoAcid] = AltAminoAcid match {
		case End(altBases) => altBases
		case _ => ProteinBases
	}
}


/**
 * Associated object for [[ProteinMap]] containing extension methods.
 */
object ProteinMap {
	extension (list: List[ProteinMap]) {
		/**
		 * Takes the list of `ProteinMap` returns a list of [[utils.protein.AminoAcid AminoAcid]] 
		 * getting the start alt amino acids for the start and appending the defaults for the rest,
		 * getting the start amino acid.
		 *
		 * @return The start amino acid chain (protein).
		 */
		def startSequence: List[AminoAcid] = list.headOption match {
			case Some(start) =>
				val res = start.getAltIfStart :: list.tail.map(_.ProteinBases)
				res.flatten
			case _ => List()
		}
		
		/**
		 * Takes the list of `ProteinMap` returns a list of [[utils.protein.AminoAcid AminoAcid]] getting the end
		 * alt amino acids for the start and appending the defaults for the rest,
		 * getting the end amino acid.
		 *
		 * @return The end amino acid chain (protein).
		 */
		def endSequence: List[AminoAcid] = list.headOption match {
			case Some(start) =>
				val res = start.getAltIfEnd :: list.tail.map(_.ProteinBases)
				res.flatten
			case _ => List()
		}
		
		/**
		 * Takes the list of `ProteinMaps` and returns all the [[utils.protein.AminoAcid AminoAcid]]
		 * in each appended into a list.
		 *
		 * @return The amino acid list (protein).
		 */
		def allSequences: List[AminoAcid] = list.flatMap(_.ProteinBases)
	}
}


/**
 * A top level class containing a list of [[ProteinMap]] and access methods to allow to
 * quickly search for map objects for given mRNA codon values (a codon being a number
 * from 0-63 representing a group of 3 [[utils.mrna.MRNABases MRNABases]]).
 *
 * @param mappings A list of map objects associating each codon values with a list of
 *                 [[utils.protein.AminoAcid AminoAcid]] (a protein).
 */
class ProteinMappings(mappings: List[ProteinMap] = List()) {
	/**
	 * A list of map objects associating each codon values with a list of 
	 * [[utils.protein.AminoAcid AminoAcid]] (a protein).
	 * Defaults to [[ProteinMappings.DefaultMappings]].
	 */
	val Mappings: List[ProteinMap] =
		if(mappings.isEmpty) ProteinMappings.DefaultMappings
		else mappings
	
	/**
	 * Tries to find a [[ProteinMap]] that contains a given codon.
	 *
	 * @param codon The codon to search for.
	 * @return The related [[ProteinMap]], returns `None` if no map is found for the codon.
	 */
	def getAminoAcidFromCodon(codon: Int): Option[ProteinMap] =
		Mappings.find(_.hasCodon(codon))
	
	/**
	 * Finds a list of mappings for a given list of codons, returns `None` if there
	 * is no related map for any of the given codon values.
	 *
	 * @param codons A given list of codon values.
	 * @return The attempted find list of related ProteinMaps.
	 */
	def getAminoAcidsFromCodonList(codons: List[Int]): Option[List[ProteinMap]] = {
		val res = for ( codon <- codons ) yield {
			getAminoAcidFromCodon(codon) match {
				case Some(map) => map
				case None => return None
			}
		}
		Some(res)
	}
}

/**
 * Associated object for [[fasta.ProteinMappings ProteinMappings]] containing associated functions
 * and type casts.
 */
object ProteinMappings {
	/**
	 * The default mappings for converting between [[utils.mrna.MRNABases MRNABases]] and 
	 * [[utils.protein.AminoAcid AminoAcid]].
	 */
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
}
