package org.vulpinedesigns.initiator_set
package utils.kozak

import scala.language.implicitConversions

/**
 * A helper class containing calculated data (namely [[utils.kozak.KzContext.Strength KzContext.Strength]])
 * namely from checking a list of [[utils.mrna.MRNABases MRNABases]] against [[utils.kozak.KzConsensus KzConsensus]], I.E.
 * via [[utils.kozak.KzConsensus.similarity() KzConsensus.similarity]].
 *
 * @param ContextStart The start of the [[utils.mrna.MRNABases MRNABases]] sequence that has been checked.
 * @param ContextEnd The end of the [[utils.mrna.MRNABases MRNABases]] sequence that has been checked.
 * @param InitiatorStart The index in the [[utils.mrna.MRNABases MRNABases]] sequence that has been checked.
 * @param Strength The resultant calculated strength (I.E. through [[utils.kozak.KzConsensus.Sequence KzConsensus.Sequence]].
 * @param Consensus The consensus used to calculate the values.
 */
class KzContext(
       val ContextStart: Int,
       val ContextEnd: Int,
       val InitiatorStart: Int,
       val Strength: Double,
       val Consensus: KzConsensus
)

/**
 * Associated object for [[utils.kozak.KzContext KzContext]], containing implicit type castings.
 */
object KzContext {
	implicit def toString(context: KzContext): String = {
		val consensus: String = context.Consensus
		s"${context.ContextStart}..${context.ContextEnd}=${context.Strength} ($consensus)"
	}
}
