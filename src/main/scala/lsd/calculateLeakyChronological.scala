package org.vulpinedesigns.initiator_set
package lsd

import utils.kozak.KzContext
import utils.mrna.mRNA

/**
 * Calculates the averages for each [[mRNA.AdjustedWeights]] and [[KzContext.Strength]] in
 * [[mRNA.KzContexts]], and can also apply a list of penalties with on each item, passing in
 * the [[KzContext.ContextStart]] for each item.
 *
 * A result of 0 means the [[KzContext]] has a been compared such that it matches and nothing
 * has been returned from the penalties.
 *
 * @param mrna The mRNA containing the weights [[mRNA.BaseWeights]] to be adjusted.
 * @param penalties A list of functions that can apply a penalty based on [[KzContext.ContextStart]].
 * @return The "leakiness" score for each position, returns `None` if there is fewer
 *         [[mRNA.AdjustedWeights]] than [[mRNA.KzContexts]].
 */
def calculateLeakyChronological(mrna: mRNA, penalties: List[Int => Double]): Option[List[Double]] = {
	val kzContexts = mrna.KzContexts
	val adjustedWeights = mrna.AdjustedWeights
	val res = for (
		(context, index) <- kzContexts.zipWithIndex
	) yield {
		val weight = if(adjustedWeights.contains(index)) adjustedWeights(index)
		else return None
		makeLeakyValue(context, weight, penalties)
	}
	Some(res)
}

private def makeLeakyValue(context: KzContext, relativeStrength: Double, penalties: List[Int => Double]): Double = {
	val penaltiesLength = 2 + penalties.length
	val strength = context.Strength
	val pos = context.InitiatorStart
	val combinedPenalties = penalties.map(_(pos)).sum
	1 - (relativeStrength + strength + combinedPenalties) / penaltiesLength
}
