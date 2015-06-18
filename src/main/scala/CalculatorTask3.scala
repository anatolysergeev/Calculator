import scala.math._;
import scala.collection._;
import scala.io.StdIn._;

object CalculatorTask3 {
  def main (args: Array[String]) {
    val calculator = new CalculatorTask3();

    while (true) {
      val line = readLine;
      if (line.equals("end")) return
      println(calculator.compute(line));
    }
  }
}

class CalculatorTask3 extends Calculator {
  def whiteSpace(c: Char): Boolean = c == ' ';
  def isNumber(c: Char): Boolean = c >= '0' && c <= '9';

  def parseNumber(q: mutable.Queue[Char]): BigDecimal = {
    var num: String = "";
    while (!q.isEmpty && (isNumber(q.front) || q.front == '.'))
      num += q.dequeue();
    return BigDecimal(num);
  }


  def parseOperator(q: mutable.Queue[Char]): String = {
    var op: String = "";
    while (!q.isEmpty && !whiteSpace(q.front) && !isNumber(q.front) && q.front != ')' && q.front != '(')
      op += q.dequeue();
    return op;
  }

  def compute(input: String): BigDecimal = {
    val st = new mutable.Stack[BigDecimal];
    val op = new mutable.Stack[Operator];
    val q = new mutable.Queue[Char];
    for (i <- input) q += i;

    var may_unary: Boolean = true;
    while (!q.isEmpty) {
      if (whiteSpace(q.front)) {
        q.dequeue()
      } else {
        if (isNumber(q.front)) {
          st.push(parseNumber(q));
          may_unary = false;
        } else if (q.front == '(') {
          op.push(BinaryOperator("" + q.dequeue()))
          may_unary = true;
        } else if (q.front == ')') {
          q.dequeue();
          while (!op.top.getName.equals("(")) process(st, op.pop);
          op.pop;
          may_unary = false;
        } else {
          val str = parseOperator(q);
          if (str.equals("!")) may_unary = true;
          val oper = new Operator(str, may_unary);
          while (!op.isEmpty && (Operator.leftAssoc(oper) && op.top >= oper || !Operator.leftAssoc(oper) && op.top > oper))
            process(st, op.pop());
          op.push(oper);
          if (str.equals("!")) may_unary = false;
          else may_unary = true;
        }
      }
    }
    while (!op.isEmpty) process(st, op.pop);
    return st.top;
  }


  def process(st: mutable.Stack[BigDecimal], op: Operator) {
    if (op.isUnary) {
      val x = st.pop();
      st.push(op.unary_op(x));
    } else {
      val r = st.pop();
      var l = st.pop();
      st.push(op.binary_op(l, r));
    }
  }
}

