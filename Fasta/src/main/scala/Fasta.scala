package org.vulpine_designs.initiator_set.fasta

import org.vulpine_designs.initiator_set.utils.protein.AminoAcid
import org.vulpine_designs.initiator_set.utils.mRNA.{
	MRNABases,
	indexCodons
}
import ProteinMap.{
	allSequences,
	startSequence,
	endSequence
}

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
