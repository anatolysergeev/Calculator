import scala.math.BigDecimal

object Operator {
  val plus = { (x: BigDecimal, y: BigDecimal) => x + y }
  val minus = { (x: BigDecimal, y: BigDecimal) => x - y }
  val mul = { (x: BigDecimal, y: BigDecimal) => x * y }
  val div = { (x: BigDecimal, y: BigDecimal) => x / y }
  val pow = { (x: BigDecimal, y: BigDecimal) => x.pow(y.toInt) }
  val sin = { (x: BigDecimal) => BigDecimal(math.sin(x.toDouble)) }
  val cos = { (x: BigDecimal) => BigDecimal(math.cos(x.toDouble)) }
  val fact = (x: BigDecimal) => BigDecimal((1 to x.toInt).foldLeft(1)(_ * _))
  val unary_minus = { (x: BigDecimal) => -1 * x }

  def leftAssoc(op: Operator): Boolean = {
    if (op.isUnary) return false;
    return !op.getName.equals("^");
  }


  def whatPriority(name: String, unary: Boolean): Int = {
    if (name.equals("!")) return 5;
    if (name.equals("^")) return 4;
    if (unary) return 3;
    if (name.equals("*") || name.equals("/")) return 2;
    if (name.equals("+") || name.equals("-")) return 1;
    return 0;
  }

  def whatBinaryFunction(name: String): ((BigDecimal, BigDecimal) => BigDecimal) = {
    name match {
      case "*" => mul
      case "/" => div
      case "-" => minus
      case "+" => plus
      case "^" => pow
      case _ => null
    }
  }

  def whatUnaryFunction(name: String): (BigDecimal) => BigDecimal = {
    name match {
      case "sin" => sin
      case "cos" => cos
      case "!" => fact
      case "-" => unary_minus
      case _ => null
    }
  }
}

class Operator(name: String, unary: Boolean) {
  val priority = Operator.whatPriority(name, unary);
  val binary_op = Operator.whatBinaryFunction(name);
  val unary_op = Operator.whatUnaryFunction(name);
  def isUnary = unary;
  def getName = name;

  def equals(other: Operator): Boolean = {
    return this.name.equals(other.getName);
  }

  def >=(other: Operator): Boolean = {
    return this.priority >= other.priority;
  }

  def >(other: Operator): Boolean = {
    return this.priority > other.priority;
  }

  override def toString(): String = {
    return "<" + name + " " + isUnary + " " + priority + ">";
  }
}