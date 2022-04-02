package org.vulpine_designs.initiator_set.utils
package fasta

import mRNA.MRNABases
import DNA.DNABases
import protein.AminoAcid

/**
 * A base class for a tokenized Fasta file sequence. 
 */
sealed abstract class FastaSequence

/**
 * A case class representing a tokenized mRNA fasta file sequence. 
 * 
 * @param seq
 */
case class mRNASeq(seq: List[MRNABases]) extends FastaSequence

/**
 * A case class representing a tokenized DNA fasta file sequence. 
 *
 * @param seq
 */
case class DNASeq(seq: List[DNABases]) extends  FastaSequence

/**
 * A case class representing a tokenized amino acid (protein) fasta file sequence.
 * 
 * @param seq
 */
case class ProteinSeq(seq: List[AminoAcid]) extends FastaSequence

/**
 * A class representing a tokenized fasta file, containing the tokenized sequence, the ID and the raw
 * string sequence. 
 * 
 * @param IdString `String`: the ID of the sequence from the file 
 * @param Sequence `String`: the raw string of the sequence from the file
 * @param Type `FastaSequence`: the tokenized sequence from the file 
 */
class Fasta(
	val IdString: String,
	val Sequence: String,
	val Type: FastaSequence
)
