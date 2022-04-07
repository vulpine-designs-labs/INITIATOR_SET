package org.vulpinedesigns.initiator_set
package fasta

import utils.protein.AminoAcid
import utils.mrna.MRNABases
import ProteinMap.{
	allSequences,
	startSequence,
	endSequence
}


/**
 * Contains methods for converting between types of de-serialised Fasta sequences,
 * ([[utils.dna.DNABases DNABases]], [[utils.mrna.MRNABases MRNABases]], and [[utils.protein.AminoAcid AminoAcid]]).
 */
object Convert {
	/**
	 * Takes a list of [[utils.mrna.MRNABases MRNABases]] and converts it to a list of [[utils.protein.AminoAcid AminoAcid]]
	 * (a protein) as per `ProteinMappings` parameter, this can be overridden.
	 *
	 * @param mrnaBases The list of mRNA bases to be converted.
	 * @param mapping   The [[fasta.ProteinMappings ProteinMappings]] object defining what mRNA bases convert to
	 *                  what AminoAcids.
	 * @return The converted amino acids, returns `None` if the bases cannot be converted to
	 *         their codons, or a related mapping cannot be found.
	 */
	def mRNAListToProtein(
		 mrnaBases: List[MRNABases],
		 mapping: ProteinMappings = new ProteinMappings()
	): Option[List[AminoAcid]] = {
		val codons = MRNABases.indexCodons(mrnaBases) match {
			case Some(value) => value
			case None => return None
		}
		val res = mapping.getAminoAcidsFromCodonList(codons) match {
			case Some(value) => makeProteinChain(value)
			case None => return None
		}
		Some(res)
	}
	
	private def makeProteinChain(maps: List[ProteinMap]): List[AminoAcid] = {
		val startIndex = maps.indexWhere(m => m.AltAminoAcid == Start)
		val valuesPostStart = maps.slice(startIndex, maps.length)
		val endIndex = valuesPostStart.indexWhere(m => m.AltAminoAcid == End)
		
		val valuesUntilStart = maps.slice(0, startIndex)
			.allSequences
		val valuesBetween = valuesPostStart.slice(0, endIndex + 1)
			.startSequence
		val valuesPostEnd = valuesPostStart.slice(endIndex + 1, valuesPostStart.length)
			.endSequence
		valuesUntilStart ++ valuesBetween ++ valuesPostEnd
	}
}
