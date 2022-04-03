package org.vulpine_designs.initiator_set
package fasta

import utils.protein.AminoAcid
import utils.mrna.{
	MRNABases,
	indexCodons
}
import ProteinMap.{
	allSequences,
	startSequence,
	endSequence
}


/**
 * Takes a list of [[MRNABases]] and converts it to a list of [[AminoAcid]] (a protein)
 * as per `ProteinMappings` parameter, this can be overridden.
 *
 * @param mrnaBases `List[MRNABases]`: the list of mRNA bases to be converted
 * @param mapping `ProteinMapping`: the [[ProteinMapping]] object defining what mRNA bases convert to
 *                what AminoAcids
 * @return `Option[List[AminoAcid]]`: the converted amino acids, returns `None` if
 *         the bases cannot be converted to their codons, or a related mapping cannot be found
 */
def mRNAListToProtein(
	mrnaBases: List[MRNABases],
	mapping: ProteinMappings = new ProteinMappings()
): Option[List[AminoAcid]] = {
	val codons = indexCodons(mrnaBases) match {
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
