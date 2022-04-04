package org.vulpinedesigns.initiator_set
package kozak_calc

import utils.kozak.{
	KzConsensus,
	KzContext
}
import utils.mrna.{
	deIndexCodons,
	mRNA
}

/**
 * Takes a [[mRNA]] and list of [[KzConsensus]], it will then loop over [[mRNA.Codons]],
 * finding a [[KzConsensus]] from the `kozaks` param with a matching [[KzConsensus.indexedCodon]], it will
 * then get the similarity score between the mrna and the selected [[KzConsensus]]. Storing & returning
 * the results and data into a list of [[KzContext]].
 *
 * @param mrna The mrna to be checked.
 * @param kozaks A list of Kozak Consensus Sequences to get the similarity score.
 * @return A list containing the results of each similarity check.
 */
def calculateKozaks(mrna: mRNA, kozaks: List[KzConsensus]): List[KzContext] = {
	val kzStartCodons = kozaks map { kz =>
		kz.indexedCodon.getOrElse(0)
	}
	val contexts = 0 to (mrna.Codons.length -3) flatMap { index =>
		val startCodon = mrna.Codons(index)
		val kozakIndex = kzStartCodons.indexOf(startCodon)
		kozaks.lift(kozakIndex) match {
			case Some(kozak) => makeKzContext(index, kozak, mrna)
			case None => None
		}
	}
	contexts.toList
}

private def makeKzContext(index: Int, kozak: KzConsensus, mrna: mRNA): Option[KzContext] = {
	val leadingLength = kozak.CodonStart
	val contextStart = index + leadingLength
	val contextEnd = contextStart + kozak.Sequence.length
	val mrnaCode = deIndexCodons(mrna.Codons)
	if(contextStart < 0 || contextEnd > mrnaCode.length)
		return None
	val similarityArea = mrnaCode.slice(contextStart, contextEnd)
	val similarity = kozak.similarity(similarityArea)
	
	Some(new KzContext(
		ContextStart = contextStart,
		ContextEnd = contextEnd,
		InitiatorStart = index,
		Strength = similarity,
		Consensus = kozak
	))
}
