package org.vulpinedesigns.initiator_set

import utils.mrna.MRNABases


/**
 * Contains a series of methods used for extracting and handling weights to codon values.
 *
 * Where a codon is an `Int` between 0-63 representing all the possible combinations of
 * 3 [[utils.mrna.MRNABases MRNABases]].
 */
object map_aic {
	/**
	 * A function that will take a map of integers and doubles, and will normalize all the
	 * doubles to be between 0 and 1 (where 1 is the largest value in the map).
	 *
	 * The integers represent a codon, this is an integer between 0 and 63, that is the
	 * total possible combinations of 3 [[utils.mrna.MRNABases MRNABases]].
	 * See [[utils.mrna.MRNABases.indexCodon() MRNABases.indexCodon]] and
	 * [[utils.mrna.MRNABases.deIndexCodon() MRNABases.deIndexCodon]].
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
	
	/**
	 * This will take a string from a file containing codons (groups of 3 [[utils.mrna.MRNABases MRNABases]])
	 * and the weight of each one on each line and deserializes them, including using
	 * [[utils.mrna.MRNABases.indexCodon() MRNABases.indexCodon]] to deserialize the codon.
	 *
	 * Will return `None` if a codon or weight is malformed.
	 *
	 * @example
	 * `AUG 12.3 # Hello World`
	 * Will become:
	 * `Map(12 -> 12.3)`
	 *
	 * @param input The input string to be parsed.
	 * @return The tokenized codon values + weights.
	 */
	def extractCodonWeights(input: String): Option[Map[Int, Double]] = {
		val inputLines = splitFilterLines(input)
		extractCodonAndWeights(inputLines)
	}
	
	private def splitFilterLines(input: String): List[String] = {
		val res = input.split('\n')
			.filter(_.length > 3)
			.flatMap { line =>
				val s = line.split('#').head.filter(_ != ' ')
				if(s.isEmpty) None
				else Some(s)
			}
		res.toList
	}
	
	private def extractCodonAndWeights(lines: List[String]): Option[Map[Int, Double]] = {
		val codons = extractCodons(lines) match {
			case Some(c) => c
			case None => return None
		}
		val weights = extractWeights(lines) match {
			case Some(w) => w
			case None => return None
		}
		val res = codons.zip(weights)
			.map((c, w) => c -> w)
			.toMap
		Some(res)
	}
	
	private def extractCodons(lines: List[String]): Option[List[Int]] = {
		val res = for (line <- lines) yield {
			val codonStr = line.slice(0, 3)
			val codonBases = MRNABases.listFromString(codonStr) match {
				case Some(codons) => codons
				case None => return None
			}
			MRNABases.indexCodon(codonBases) match {
				case Some(index) => index
				case None => return None
			}
		}
		Some(res)
	}
	
	private def extractWeights(lines: List[String]): Option[List[Double]] = {
		val res = for (line <- lines) yield {
			val weightStr = line.slice(3, line.length).filter(_ != ' ')
			weightStr.toDoubleOption match {
				case Some(weight) => weight
				case None => return None
			}
		}
		Some(res)
	}
}

object Main extends App {
	val x = map_aic.extractCodonWeights("AUG 12.3")
		.getOrElse(Map())
		.toList
		.head._2
	println(s"${x}")
}
