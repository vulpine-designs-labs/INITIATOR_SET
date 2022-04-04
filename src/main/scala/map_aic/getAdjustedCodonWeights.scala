package org.vulpinedesigns.initiator_set
package map_aic

import utils.mrna.{
	MRNABases,
	indexCodon,
	deIndexCodon
}


/**
 * A function that will take a map of integers and doubles, and will normalize all the
 * doubles to be between 0 and 1 (where 1 is the largest value in the map).
 *
 * The integers represent a codon, this is an integer between 0 and 63, that is the
 * total possible combinations of 3 [[utils.mrna.MRNABases MRNABases]].
 * See [[utils.mrna.indexCodon() indexCodon]] and [[utils.mrna.deIndexCodon() deIndexCodon]].
 *
 * @param baseWeights The input map of values.
 * @return The list of adjusted values.
 */
def getAdjustedCodonWeights(baseWeights: Map[Int, Double]): Map[Int, Double] = {
	val maxWeight = baseWeights.toList.maxBy((c, w) => w)._2
	val adjustedList: List[(Int, Double)] = baseWeights.toList map { (codon, weight) =>
		if(maxWeight == 0) (codon, 0.0)
		else (codon, weight / maxWeight)
	}
	adjustedList.map((c, w) => c -> w).toMap
}
