package Scala

import java.util.Random

/**
  * Created by Hassan on 4/16/16.
  */
trait Expression

//Expression used to represent whole numbers
case class IExpr(integer: Int) extends Expression {
  //override def toString:String = integer+ ""
}

//a seperate case class specifically for operators to help with case matching
case class OExpr(operator: String) extends Expression {
 // override def toString:String = " "+operator+" "
}

//for the binary functions +, -, *, /
case class BExpr(op: OExpr, left: Expression, right: Expression) extends Expression{
  //override def toString:String = left.toString + op.toString + right.toString
}


object ExpressionCreator {
  def apply = {
    this
  }

  //creates a random operator
  private def randomOperator: OExpr = new Random().nextInt(4) match {
    case 0 => OExpr("+")
    case 1 => OExpr("-")
    case 2 => OExpr("*")
    case 3 => OExpr("/")
  }

  //creates a random Int
  private def randomInt: IExpr = IExpr(new Random().nextInt(10) +1)

  //returns us a random Expression using our private methods
  def randomExpression: Expression = {
    def generator(n: Int): Expression = new Random().nextInt(2) match {
      case 0 => randomInt
      case 1 if n > 0 => BExpr(randomOperator, generator(n - 1), generator(n - 1))
      case 1 => randomInt
    }
    generator(3)
  }

}


object EEvaluator {

  def apply(expression: Expression): Int = expression match {
    case IExpr(i) => i
    case BExpr(b, l, r) => b.operator match {
      case "+" => apply(l) + apply(r)
      case "-" => apply(l) - apply(r)
      case "/"  if apply(r)>0 => apply(l) / apply(r)
      case "/" => apply(l) * 0
      case "*" => apply(l) * apply(r)
    }
  }
}