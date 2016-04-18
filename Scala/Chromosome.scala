package Scala

import scala.collection.mutable.ArrayBuffer
import java.util.Random


/**
  * Created by Hassan on 4/16/16.
  */

class Gene(val n: Expression, val fitness: Float)


class Chromosome(var list: List[Gene], val target: Float, val totFitness: Double)

object Chromosome {
  val eval = EEvaluator
  val ec = ExpressionCreator
  val r = new Random()

  def apply(target: Int): Chromosome = {
    val genes: List[Gene] = generateRandom().map((e) => new Gene(e, eval(e) / target.toFloat))

    new Chromosome(genes.sortWith(_.fitness < _.fitness), target, genes.foldLeft(0.0)((x, y) => y.fitness + x) / target)
  }

  def apply(list: List[Gene], target: Int) = {
    new Chromosome(list, target, list.foldLeft(0.0)((x, y) => y.fitness + x) / target)
  }


  private def generateRandom(): List[Expression] = {
    def loop( acc: List[Expression]): List[Expression] = {
      var ac = acc
      for (i <- 0 to 10)
        ac =ac.::(ec.randomExpression)
      ac
    }
    loop(List())
  }

  def swap(x: (Chromosome, Chromosome), target: Int): List[Chromosome] = {
    val pivot: Int = r.nextInt(x._1.list.length)
    val a = x._1.list.splitAt(pivot)
    val b = x._2.list.splitAt(pivot)

    //the second region of c1 swapped for the second region of c2
    val c = Chromosome(a._1 ++ b._2, target)
    //the first region of the chromosome c1 swapped for the first region of c2
    val d = Chromosome(b._1 ++ a._2, target)
    List(c, d)
  }

  def selectParents(pool: List[Chromosome]): (Chromosome, Chromosome) = {
    var parents: List[Chromosome] = List()
    for (i <- 0 to 1) {
      var bestFit = pool(r.nextInt(pool.size))
      for (i <- 1 to 3) {
        val j = r.nextInt(pool.size)
        if (pool(j).totFitness < bestFit.totFitness) {
          bestFit = pool(j)
        }
      }
      parents = parents :+ bestFit
    }
    val x = parents.sortWith(_.totFitness > _.totFitness).take(2)
    (x.head, x(1))
  }

  def mutate(chromosome: Chromosome): Chromosome = {
    val osize = chromosome.list.size
    def gen(start: Int, end: Int, list: ArrayBuffer[Gene]): List[Gene] = {
      //arrayBuffer is mutable so this causes changes to list
      list.remove(start, end)
      var x = list.toList
      val nsize = x.size
      for (i <- 0 to (osize - nsize)) {
        val s = ec.randomExpression
        x = x :+ new Gene(s, eval(s) / chromosome.target)
      }
      x
    }

    val start = r.nextInt(osize)
    val end = r.nextInt(osize)
    if (start < end)
      chromosome.list = gen(start, end-start, chromosome.list.to[ArrayBuffer])
    chromosome
  }

}





