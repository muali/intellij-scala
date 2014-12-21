package org.jetbrains.plugins.scala.lang.psi.impl.expr

import com.intellij.lang.ASTNode
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.ScTypePolymorphicType
import org.jetbrains.plugins.scala.lang.psi.types.result.{Success, TypeResult, TypingContext}
import org.jetbrains.plugins.scala.lang.psi.{ScalaPsiElementImpl, ScalaPsiUtil}

/**
 * @author Maxim Moskvitin
 * Date: 20.12.2014
 */
class ScGenericOperatorImpl (node: ASTNode) extends ScalaPsiElementImpl(node) with ScGenericOperator {
  override def toString: String = "GenericOperator"

  private def convertReferencedType(typeResult: TypeResult[ScType]): TypeResult[ScType] = {
    val refType = typeResult.getOrElse(return typeResult)
    refType match {
      case ScTypePolymorphicType(int, tps) =>
        val subst = ScalaPsiUtil.genericCallSubstitutor(tps.map(p => (p.name, ScalaPsiUtil.getPsiElementId(p.ptp))), arguments)
        Success(subst.subst(int), Some(this))
      case _ => Success(refType, Some(this))
    }
  }

  protected override def innerType(ctx: TypingContext): TypeResult[ScType] = {
    val typeResult = referencedExpr.getNonValueType(ctx)
    convertReferencedType(typeResult)
  }

  def shapeMultiType: Array[TypeResult[ScType]] = {
    val typeResult: Array[TypeResult[ScType]] = referencedExpr.shapeMultiType
    typeResult.map(convertReferencedType)
  }

  def multiType: Array[TypeResult[ScType]] = {
    val typeResult: Array[TypeResult[ScType]] = referencedExpr.multiType
    typeResult.map(convertReferencedType)
  }

}