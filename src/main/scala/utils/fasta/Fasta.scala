package org.vulpinedesigns.initiator_set
package utils.fasta

import utils.protein.AminoAcid
import utils.mrna.MRNABases
import utils.dna.DNABases


/**
 * A base class for a tokenized Fasta file sequence. 
 */
sealed abstract class FastaSequence

/**
 * A case class representing a tokenized mRNA fasta file sequence. 
 *
 * @param seq The list of [[utils.mrna.MRNABases MRNABases]].
 */
case class mRNASeq(seq: List[MRNABases]) extends FastaSequence

/**
 * A case class representing a tokenized DNA fasta file sequence. 
 *
 * @param seq The list of [[utils.dna.DNABases DNABases]].
 */
case class DNASeq(seq: List[DNABases]) extends  FastaSequence

/**
 * A case class representing a tokenized amino acid (protein) fasta file sequence.
 *
 * @param seq The list of [[utils.protein.AminoAcid AminoAcid]] (a protein).
 */
case class ProteinSeq(seq: List[AminoAcid]) extends FastaSequence

/**
 * A class representing a tokenized fasta file, containing the tokenized sequence, the ID and the raw
 * string sequence. 
 *
 * @param IdString The ID of the sequence from the file.
 * @param Sequence The raw string of the sequence from the file.
 * @param Type The tokenized sequence from the file.
 */
class Fasta(
	val IdString: String,
	val Sequence: String,
	val Type: FastaSequence
)
