package org.vulpine_designs.initiator_set

import utils.mRNA._
import utils.kozak._

object main extends App {
	
	val kzNucleotide = new KzNucleotide(
		A = 0.7,
		U = 0.7,
		G = 0.2,
		C = 0.2,
		Importance = 1
	)
	
	val kzNucleotideStr: String = kzNucleotide
	
	println(kzNucleotideStr)
	
}
