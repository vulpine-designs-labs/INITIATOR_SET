package org.vulpine_designs.initiator_set.utils
package kozak

import scala.language.implicitConversions

/**
 * A helper class containing calculated data (namely [[KzContext.Strength]])
 * namely from checking a list of [[mRNA.MRNABases]] against [[KzConsensus]], I.E.
 * via [[KzConsensus.similarity]].
 *
 * @param ContextStart   : `Int` the start of the [[mRNA.MRNABases]] sequence that has been checked.
 * @param ContextEnd     : `Int` the end of the [[mRNA.MRNABases]] sequence that has been checked.
 * @param InitiatorStart : `Int` the index in the [[mRNA.MRNABases]] sequence that has been checked.
 * @param Strength       : `Double` the resultant calculated strength (I.E. through [[KzConsensus.Sequence]].
 * @param Consensus      : [[KzConsensus]] the consensus used to calculate the values.
 */
class KzContext(
		val ContextStart: Int,
		val ContextEnd: Int,    
		val InitiatorStart: Int,
		val Strength: Double,
		val Consensus: KzConsensus
)

/**
 * Associated object for [[KzContext]], containing implicit type castings.
 */
object KzContext {
	implicit def toString(context: KzContext): String = {
		val consensus: String = context.Consensus
		s"${context.ContextStart}..${context.ContextEnd}=${context.Strength} ($consensus)"
	}
}
