package org.jetbrains.plugins.scala
package lang
package psi
package impl
package expr

import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElementVisitor
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.ScalaElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.ScTypePolymorphicType
import org.jetbrains.plugins.scala.lang.psi.types.result.{Success, TypeResult, TypingContext}

import scala.collection.Seq

/**
 * @author Alexander Podkhalyuzin
 * Date: 06.03.2008
 */

class ScInfixExprImpl(node: ASTNode) extends ScalaPsiElementImpl(node) with ScInfixExpr {
  override def toString: String = "InfixExpression"

  override def argumentExpressions: Seq[ScExpression] = {
    if (isLeftAssoc) Seq(lOp) else rOp match {
      case tuple: ScTuple => tuple.exprs
      case t: ScParenthesisedExpr => t.expr match {
        case Some(expr) => Seq(expr)
        case None => Seq(t)
      }
      case expr => Seq(expr)
    }
  }

  private def convertReferencedType(typeResult: TypeResult[ScType]): TypeResult[ScType] = {
    val refType = typeResult.getOrElse(return typeResult)
    refType match {
      case ScTypePolymorphicType(int, tps) =>
        val subst = ScalaPsiUtil.genericCallSubstitutor(tps.map(p => (p.name, ScalaPsiUtil.getPsiElementId(p.ptp))), typeArguments)
        Success(subst.subst(int), Some(this))
      case _ => Success(refType, Some(this))
    }
  }

  protected override def innerType(ctx: TypingContext): TypeResult[ScType] = {
    def cacheBaseParts(inf: ScInfixExpr): Unit = {
      inf.getBaseExpr match {
        case inf: ScInfixExpr =>
          cacheBaseParts(inf)
        case _ =>
      }
      inf.getBaseExpr.getType(TypingContext.empty)
    }

    cacheBaseParts(this)

    operation.bind() match {
      //this is assignment statement: x += 1 equals to x = x + 1
      case Some(r) if r.element.name + "=" == operation.refName =>
        super.innerType(ctx)
        val lText = lOp.getText
        val rText = rOp.getText
        val exprText = s"$lText = $lText ${r.element.name} $rText"
        val newExpr = ScalaPsiElementFactory.createExpressionWithContextFromText(exprText, getContext, this)
        newExpr.getType(TypingContext.empty)
      case _ => super.innerType(ctx)
    }
  }

  def shapeMultiType: Array[TypeResult[ScType]] = {
    val typeResult: Array[TypeResult[ScType]] = operation.shapeMultiType
    typeResult.map(convertReferencedType)
  }

  def multiType: Array[TypeResult[ScType]] = {
    val typeResult: Array[TypeResult[ScType]] = operation.multiType
    typeResult.map(convertReferencedType)
  }

  override def accept(visitor: ScalaElementVisitor) {
    visitor.visitInfixExpression(this)
  }

  override def accept(visitor: PsiElementVisitor) {
    visitor match {
      case visitor: ScalaElementVisitor => visitor.visitInfixExpression(this)
      case _ => super.accept(visitor)
    }
  }
}