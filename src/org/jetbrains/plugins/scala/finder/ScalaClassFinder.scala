package org.jetbrains.plugins.scala
package finder

import _root_.org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.ScSyntheticPackage
import _root_.scala.collection.mutable.ArrayBuffer
import caches.ScalaCachesManager
import com.intellij.openapi.components.ProjectComponent
import com.intellij.openapi.project.Project
import com.intellij.psi.search.GlobalSearchScope
import lang.psi.api.ScalaFile
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.openapi.roots.{ProjectRootManager, ProjectFileIndex}
import com.intellij.openapi.fileTypes.{StdFileTypes, FileType}
import com.intellij.openapi.module.Module
import com.intellij.psi._
import impl.file.PsiPackageImpl
import impl.{JavaPsiFacadeImpl, PsiManagerEx}
import com.intellij.openapi.util.Comparing
import lang.psi.api.toplevel.typedef.ScTypeDefinition
import lang.psi.impl.{ScPackageImpl, ScalaPsiManager}
import java.lang.String
import java.util.{Set, ArrayList}

class ScalaClassFinder(project: Project) extends PsiElementFinder {
  def findClasses(qualifiedName: String, scope: GlobalSearchScope): Array[PsiClass] = {
    Array.empty
  }

  def findClass(qualifiedName: String, scope: GlobalSearchScope): PsiClass = {
    null
  }

  override def findPackage(qName: String): PsiPackage =
    ScalaPsiManager.instance(project).syntheticPackage(qName)

  override def getClassNames(psiPackage: PsiPackage, scope: GlobalSearchScope): Set[String] = {
    ScalaCachesManager.getInstance(project).getNamesCache.getClassNames(psiPackage, scope)
  }
}