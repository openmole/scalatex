package scalatex
package stages

import scalaparse.Scala._
import scalaparse.syntax._
import fastparse._, NoWhitespace._

/**
 * Parses the input text into a roughly-structured AST. This AST
 * is much simpler than the real Scala AST, but serves us well
 * enough until we stuff the code-strings into the real Scala
 * parser later
 */
object Parser extends ((String, Int) => Parsed[Ast.Block]){
  def apply(input: String, offset: Int = 0): Parsed[Ast.Block] = {
    parse(input, new Parser(offset).File(_))
  }
}

class Parser(indent: Int = 0) {
  import scalaparse.syntax.{Key => K}

  /**
   * This only needs to parse the second `@`l the first one is
   * already parsed by [[BodyItem]]
   */
  def `@@`[_: P] = P( Index ~ "@" ).map(Ast.Block.Text(_, "@"))

  def TextNot[_: P](chars: String) = {
    def AllowedChars = CharsWhile(!(chars + "@\n").contains(_))
    P(Index ~ AllowedChars.!.rep(1)).map {
      case (i, x) => Ast.Block.Text(i, x.mkString)
    }
  }

  def Text[_: P] = TextNot("")
  def Code[_: P] = P( (scalaparse.syntax.Identifiers.Id | BlockExpr | ExprCtx.Parened ).! )
  def Header[_: P] = P( (BlockDef | Import).! )

  def HeaderBlock[_: P] = P( Index ~ Header ~ (WL.! ~ "@" ~ Header).rep ~ Body ).map{
    case (i, start, heads, body) => Ast.Header(i, start + heads.map{case (x, y) => x + y}.mkString, body)
  }

  def BlankLine[_: P] = P( "\n" ~ " ".rep ~ &("\n") )

  def IndentSpaces[_: P] = P( " ".rep(min = indent, sep = Pass) )
  def Indent[_: P] = P( "\n" ~ IndentSpaces )
  def IndentPrefix[_: P] = P( Index ~ (Indent | Start).! ).map(Ast.Block.Text.tupled)
  def IndentScalaChain[_: P] = P(ScalaChain ~ (IndentBlock | BraceBlock).?).map{
    case (chain, body) => chain.copy(parts = chain.parts ++ body)
  }

  def Deeper[_: P] = P( " ".rep(indent + 1) )
  def IndentBlock[_: P] = {
    ("\n" ~ Deeper.!).flatMapX { i =>
      val size = i.size
      val p = implicitly[P[_]]
      p.freshSuccessUnit(p.index - (size + 1))
      new Parser(indent = size).Body //actor.rep(1, sep = ("\n" + " " * i)./)
    }
  }

  def IfHead[_: P] = P( (`if` ~/ "(" ~ ExprCtx.Expr ~ ")").! )
  def IfSuffix[_: P] = P( BraceBlock ~ (K.W("else") ~/ BraceBlock).?  )
  def IfElse[_: P] = P( Index ~ IfHead ~ IfSuffix).map { case (w, a, (b, c)) => Ast.Block.IfElse(w, a, b, c) }
  def IfBlockSuffix[_: P] = P( IndentBlock ~ (Indent ~ K.W("@else") ~ (BraceBlock | IndentBlock)).? )

  def IndentIfElse[_: P] = {
    P(Index ~ IfHead ~ (IfBlockSuffix | IfSuffix)).map {
      case (w, a, (b, c)) => Ast.Block.IfElse(w, a, b, c)
    }
  }

  def ForHead[_: P] = {
    def ForBody = P( "(" ~/ ExprCtx.Enumerators ~ ")" | "{" ~/ StatCtx.Enumerators ~ "}" )
    P( Index ~ (`for` ~/ ForBody).! )
  }
  def ForLoop[_: P] = P( ForHead ~ BraceBlock ).map(Ast.Block.For.tupled)
  def IndentForLoop[_: P] = P(
    (ForHead ~ (IndentBlock | BraceBlock)).map(Ast.Block.For.tupled)
  )

  def ScalaChain[_: P] = P( Index ~ Code ~ TypeAndArgs ~ Extension.rep.map(_.flatten) ).map {
    case (x, c, ta, ex) => Ast.Chain(x, c, ta ++ ex)
  }

  def TypeArgsVal[_: P] = (Index ~ TypeArgs.!)
  def ParenArgListVal[_: P] = (Index ~ ParenArgList.!)
  def TypeAndArgs[_: P] = (TypeArgsVal.rep ~ ParenArgListVal.rep).map {
    case (oT, oA) =>
      oT.map { case (iT, t) =>  Ast.Chain.TypeArgs(iT, t) } ++
        oA.map { case(iA, a) => Ast.Chain.Args(iA, a) }
  }

  def Extension[_: P] = {
    def InFixCallId = (Index ~ "." ~ Identifiers.Id.! ~ &("."))

    def InFixCall =
      (InFixCallId ~ TypeArgsVal.rep ~ ParenArgListVal.rep).map {
        case (iF, f, oT, oA) =>
          Seq(Ast.Chain.Prop(iF, f)) ++
            oT.map { case (iT, t) =>  Ast.Chain.TypeArgs(iT, t) } ++
            oA.map { case(iA, a) => Ast.Chain.Args(iA, a) }
      }

    def PostFixCallId = (Index ~ "." ~ Identifiers.Id.!)

    def PostFixCall =
      (PostFixCallId ~ TypeArgsVal.rep ~ ParenArgListVal.rep).map {
        case (iF, f, oT, oA) =>
          Seq(Ast.Chain.Prop(iF, f)) ++
            oT.map { case (iT, t) =>  Ast.Chain.TypeArgs(iT, t) } ++
            oA.map { case(iA, a) => Ast.Chain.Args(iA, a) }
      }

    P(
      // Not cutting after the ".", because full-stops are very common
      // in english so this results in lots of spurious failures
      InFixCall | PostFixCall | BraceBlock.map(Seq(_))
    )
  }
  def BraceBlock[_: P] = P( "{" ~/ BodyNoBrace  ~ "}" )

  def CtrlFlow[_: P] = P( ForLoop | IfElse | ScalaChain | HeaderBlock | `@@` ).map(Seq(_))

  def CtrlFlowIndented[_: P] = P( IndentForLoop | IndentScalaChain | IndentIfElse | HeaderBlock | `@@` )

  def IndentedExpr[_: P] = P(
    (IndentPrefix ~ "@" ~/ CtrlFlowIndented).map{ case (a, b) => Seq(a, b) }
  )
  def BodyText[_: P](exclusions: String) = P(
    TextNot(exclusions).map(Seq(_)) |
    (Index ~ Indent.!).map(Ast.Block.Text.tupled).map(Seq(_)) |
    (Index ~ BlankLine.!).map(Ast.Block.Text.tupled).map(Seq(_))
  )
  def Comment[_: P] = P(
    (Index ~ Literals.Comment.!).map { case (i, c) => Seq(Ast.Block.Comment(i, c)) }
  )

  def BodyItem[_: P](exclusions: String) : P[Seq[Ast.Block.Sub]]  = P(
    Comment | IndentedExpr |  "@" ~/ CtrlFlow | BodyText(exclusions)
  )
  def Body[_: P] = P( BodyEx("") )

  // Some day we'll turn this on, but for now it seems to be making things blow up
  def File[_: P] = P( Body/* ~ End */)
  def BodyNoBrace[_: P] = P( BodyEx("}") )
  def BodyEx[_: P](exclusions: String) =
    P( Index ~ BodyItem(exclusions).rep ).map {
      case (i, x) => Ast.Block(i, flattenText(x.flatten))
    }

  def flattenText(seq: Seq[Ast.Block.Sub]) = {
    seq.foldLeft(Seq.empty[Ast.Block.Sub]){
      case (prev :+ Ast.Block.Text(offset, txt1), Ast.Block.Text(_, txt2)) =>
        prev :+ Ast.Block.Text(offset, txt1 + txt2)
      case (prev, next) => prev :+ next
    }
  }
}

trait Ast{
  def offset: Int
}
object Ast{

  /**
   * @param parts The various bits of text and other things which make up this block
   * @param offset
   */
  case class Block(offset: Int, parts: Seq[Block.Sub]) extends Chain.Sub with Block.Sub
  object Block{
    trait Sub extends Ast
    case class Text(offset: Int, txt: String) extends Block.Sub
    case class For(offset: Int, generators: String, block: Block) extends Block.Sub
    case class IfElse(offset: Int, condition: String, block: Block, elseBlock: Option[Block]) extends Block.Sub
    case class Comment(offset: Int, str: String) extends Block.Sub
  }
  case class Header(offset: Int, front: String, block: Block) extends Block.Sub with Chain.Sub

  /**
   * @param lhs The first expression in this method-chain
   * @param parts A list of follow-on items chained to the first
   * @param offset
   */
  case class Chain(offset: Int, lhs: String, parts: Seq[Chain.Sub]) extends Block.Sub
  object Chain{
    trait Sub extends Ast
    case class Prop(offset: Int, str: String) extends Sub
    case class TypeArgs(offset: Int, str: String) extends Sub
    case class Args(offset: Int, str: String) extends Sub
  }


}
