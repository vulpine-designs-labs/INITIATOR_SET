package org.vulpine_designs.initiator_set.fasta

import org.vulpine_designs.initiator_set.utils.DNA.DNABases
import org.vulpine_designs.initiator_set.utils.fasta.{
	Fasta,
	DNASeq,
	mRNASeq,
	ProteinSeq,
	FastaSequence
}
import org.vulpine_designs.initiator_set.utils.mRNA.MRNABases
import org.vulpine_designs.initiator_set.utils.protein.AminoAcid

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
