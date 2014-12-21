package org.jetbrains.plugins.scala.lang.psi.api.expr

import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScTypeElement, ScTypeArgs}
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult

/**
 * @author Maxim Moskvitin
 * Date: 20.12.2014
 */
trait ScGenericOperator extends ScExpression {

  def referencedExpr : ScReferenceExpression = {
    val ref = findChildByClassScala(classOf[ScExpression]) match {
      case re : ScReferenceExpression => re
      case _ => throw new RuntimeException("Wrong generified operator" + getText)
    }
    ref.setContext(getContext, null)
    ref
  }

  def typeArgs = findChild(classOf[ScTypeArgs])

  def arguments : Seq[ScTypeElement] = (for (t <- typeArgs) yield t.typeArgs) match {
    case Some(x) => x
    case _ => Nil
  }

  def shapeMultiType: Array[TypeResult[ScType]]

  def shapeResolve = referencedExpr.shapeResolve

  def multiType: Array[TypeResult[ScType]]

  def multiResolve(b: Boolean) = referencedExpr.multiResolve(b)

}
