package org.vulpinedesigns.initiator_set

import utils.kozak.{
	KzConsensus,
	KzContext
}
import utils.mrna.{
	MRNABases,
	mRNA
}


/**
 * Contains methods handling kozak calculations. 
 */
object kozak_calc {
	/**
	 * Takes a [[utils.mrna.mRNA mRNA]] and list of [[utils.kozak.KzConsensus KzConsensus]], it will then loop over
	 * [[utils.mrna.mRNA.Codons mRNA.Codons]], finding a [[utils.kozak.KzConsensus KzConsensus]] from the `kozaks`
	 * param with a matching [[utils.kozak.KzConsensus.indexedCodon KzConsensus.indexedCodon]], it will then get the
	 * similarity score between the mrna and the selected [[utils.kozak.KzConsensus KzConsensus]]. Storing & returning
	 * the results and data into a list of [[utils.kozak.KzContext KzContext]].
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
		val mrnaCode = MRNABases.deIndexCodons(mrna.Codons)
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
}
