package org.vulpine_designs.initiator_set.utils
package fasta

import mRNA.MRNABases
import DNA.DNABases
import protein.AminoAcid

sealed abstract class FastaSequence

case class mRNASeq(seq: List[MRNABases]) extends FastaSequence
case class DNASeq(seq: List[DNABases]) extends  FastaSequence
case class ProteinSeq(seq: List[AminoAcid]) extends FastaSequence

class Fasta(
	val IdString: String,
	val Sequence: String,
	val Type: FastaSequence
)
