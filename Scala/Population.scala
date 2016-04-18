package Scala

import scala.util.Random

/**
  * Created by Hassan on 4/16/16.
  */
class Population(val crossoverRate: Double, val mutationRate: Double, val size: Int, target: Int) {

  private var genePool = Population.initialGenePool(size, target)

  def population = genePool

  def isDone = genePool.exists(_.list.exists(_.fitness == 1))

  def result= {
    genePool.foreach((c) => c.list.filter(_.fitness ==1).foreach((g) => println(g.n) ))
  }

  def evolve: List[Chromosome] = {
    val c = Chromosome
    genePool = (genePool collect {
      case x if Random.nextFloat() <= crossoverRate => c.swap(c.selectParents(genePool), target)
      case y if Random.nextFloat() <= mutationRate => List(c.mutate(y))
      case z => List(z)
    }).flatten.sortWith(_.totFitness > _.totFitness).take(size)
    genePool
  }


}

object Population {

  def apply(cr: Double, mr: Double, size: Int, target: Int) = new Population(cr, mr, size, target)

  private def initialGenePool(size: Int, target: Int): List[Chromosome] = {
    //the genes in each chromosome are already sorted by fitness
    List.fill(size)(Chromosome(target))
  }


}


