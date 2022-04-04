package org.vulpinedesigns.initiator_set
package fasta

import utils.protein.AminoAcid
import utils.mrna.MRNABases
import utils.dna.DNABases
import utils.fasta.{
	Fasta,
	DNASeq,
	mRNASeq,
	ProteinSeq,
	FastaSequence
}

/**
 * Takes a string read from a fasta file, breaks and parses each sequence into a list of
 * [[Fasta]] sequences. `Option[Fasta]` will be `None` if the format is invalid.
 *
 * @param sequenceString The fasta string.
 * @return The list of attempted tokenized sequences.
 */
def readFastaSequences(sequenceString: String): List[Option[Fasta]] = {
	val sequences = sequenceString.split('>').toList
	for ( seq <- sequences ) yield
		readSequence(seq)
}

private def readSequence(seq: String): Option[Fasta] = {
	val seqLines = seq.split('\n')
		.map(stringFilter).toList
	val descriptorSequence = seqLines.find(_.startsWith(">")) match {
		case Some(value) => value.tail
		case None => ""
	}
	val stringSequence = seqLines.filter(!_.startsWith(">")).mkString
	checkSeqTypeConvert(stringSequence) match {
		case Some(value) => Some(Fasta(descriptorSequence, stringSequence, value))
		case None => None
	}
}

private def stringFilter(s: String): String = {
	val commentsRemoved = s.split(';').head
	commentsRemoved.strip()
		.filter(c => c != '\n')
		.filter(c => c != '\r')
}

private def checkSeqTypeConvert(s: String): Option[FastaSequence] = {
	if(stringContainsValues(s, List("T")))
		DNABases.listFromString(s) match {
			case Some(value) => Some(DNASeq(value))
			case None => None
		}
	else if(stringContainsValues(s, List("U")))
		MRNABases.listFromString(s) match {
			case Some(value) => Some(mRNASeq(value))
			case None => None
		}
	else if(stringContainsValues(s, List("E", "F", "I", "L", "P", "Q")))
		AminoAcid.listFromString(s) match {
			case Some(value) => Some(ProteinSeq(value))
			case None => None
		}
	else None
}

private def stringContainsValues(s: String, list: List[String]): Boolean = {
	for ( li <- list ) yield {
		if(s.contains(li)) return true;
	}
	false
}
