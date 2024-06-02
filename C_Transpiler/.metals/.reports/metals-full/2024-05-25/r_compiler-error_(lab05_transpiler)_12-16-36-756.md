file://<WORKSPACE>/src/main/scala/alpine/codegen/ScalaPrinter.scala
### java.lang.AssertionError: assertion failed

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.1
Classpath:
<WORKSPACE>/.bloop/lab05_transpiler/bloop-bsp-clients-classes/classes-Metals-fbFEphUGTwqr838Gc5W45g== [exists ], <HOME>/Library/Caches/bloop/semanticdb/com.sourcegraph.semanticdb-javac.0.9.10/semanticdb-javac-0.9.10.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/sourcegraph/semanticdb-javac/0.9.10/semanticdb-javac-0.9.10.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.1/scala3-library_3-3.3.1.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.10/scala-library-2.13.10.jar [exists ]
Options:
-Xsemanticdb -sourceroot <WORKSPACE>


action parameters:
uri: file://<WORKSPACE>/src/main/scala/alpine/codegen/ScalaPrinter.scala
text:
```scala
package alpine
package codegen

import alpine.ast
import alpine.symbols
import alpine.symbols.Entity.builtinModule
import alpine.util.FatalError

import scala.annotation.tailrec
import scala.collection.mutable
import alpine.symbols.Type
import alpine.symbols.Type.Bool
import alpine.ast.Binding
import alpine.ast.Typecast
import alpine.ast.ErrorTree
import alpine.ast.ValuePattern
import alpine.ast.RecordPattern
import alpine.ast.Wildcard
import alpine.symbols.Type.Sum

/** The transpilation of an Alpine program to Scala. */
final class ScalaPrinter(syntax: TypedProgram) extends ast.TreeVisitor[ScalaPrinter.Context, Unit]:

  import ScalaPrinter.Context

  /** The program being evaluated. */
  private given TypedProgram = syntax

  /** Returns a Scala program equivalent to `syntax`. */
  def transpile(): String =
    given c: Context = Context()
    syntax.declarations.foreach(_.visit(this))
    c.typesToEmit.map(emitRecord)
    c.output.toString

  /** Writes the Scala declaration of `t` in `context`. */
  private def emitRecord(t: symbols.Type.Record)(using context: Context): Unit =
    //ALM
    t.fields.size match
        case 0 => 
          context.output ++= "case object "
          context.output ++= discriminator(t)
          context.output ++= "\n\n"
        case _ => emitNonSingletonRecord(t)(using context)

  /** Writes the Scala declaration of `t`, which is not a singleton, in `context`. */
  private def emitNonSingletonRecord(t: symbols.Type.Record)(using context: Context): Unit =
    //ALM
    context.output ++= "final case class "
    context.output ++= discriminator(t)
    context.output ++= "("
    context.output.appendCommaSeparated(t.fields.zipWithIndex) { (o, f) =>
      o ++= "$" + f._2.toString()
      o ++= ": "
      o ++= transpiledType(f._1.value)
    }
    context.output ++= ")\n\n"    

  /** Returns the transpiled form of `t`. */
  private def transpiledType(t: symbols.Type)(using context: Context): String =
    t match
      case u: symbols.Type.Builtin =>
        transpiledBuiltin(u)
      case u: symbols.Type.Record =>
        transpiledRecord(u)
      case u: symbols.Type.Arrow =>
        transpiledArrow(u)
      case u: symbols.Type.Sum =>
        transpiledSum(u)
      case _ => throw Error(s"type '${t}' is not representable in Scala")

  /** Returns the transpiled form of `t`. */
  private def transpiledBuiltin(t: symbols.Type.Builtin)(using context: Context): String =
    t match
      case symbols.Type.BuiltinModule => throw Error(s"type '${t}' is not representable in Scala")
      case symbols.Type.Bool => "Boolean"
      case symbols.Type.Int => "Int"
      case symbols.Type.Float => "Float"
      case symbols.Type.String => "String"
      case symbols.Type.Any => "Any"

  /** Returns the transpiled form of `t`. */
  private def transpiledRecord(t: symbols.Type.Record)(using context: Context): String =
    if t == symbols.Type.Unit then
      "Unit"
    else
      context.registerUse(t)
      val d = discriminator(t)
      if t.fields.isEmpty then s"${d}.type" else d

  /** Returns the transpiled form of `t`. */
  private def transpiledArrow(t: symbols.Type.Arrow)(using context: Context): String =
    val r = StringBuilder()
    r ++= "("
    r.appendCommaSeparated(t.inputs) { (o, a) => o ++= transpiledType(a.value) }
    r ++= " => "
    r ++= transpiledType(t.output)
    r ++= ")"
    r.toString()

  /** Returns the transpiled form of `t`. */
  private def transpiledSum(t: symbols.Type.Sum)(using context: Context): String =
    if t.members.isEmpty then "N" else
      t.members.map(transpiledType).mkString(" | ")

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type): String =
    t match
      case u: symbols.Type.Builtin =>
        discriminator(u)
      case u: symbols.Type.Meta =>
        s"M${discriminator(u.instance)}"
      case u: symbols.Type.Definition =>
        "D" + u.identifier
      case u: symbols.Type.Record =>
        discriminator(u)
      case u: symbols.Type.Arrow =>
        discriminator(u)
      case u: symbols.Type.Sum =>
        discriminator(u)
      case _ =>
        throw Error(s"unexpected type '${t}'")

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Builtin): String =
    t match
      case symbols.Type.BuiltinModule => "Z"
      case symbols.Type.Bool => "B"
      case symbols.Type.Int => "I"
      case symbols.Type.Float => "F"
      case symbols.Type.String => "S"
      case symbols.Type.Any => "A"

  /** Returns a string uniquely identifiyng ⁠ t ⁠ for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Record): String =
    //ALM : FINI 
    if (t == symbols.Type.Unit) then
      return "U"
    else
      val b = StringBuilder("R")
      b ++= t.identifier.drop(1)
      for f <- t.fields do
        b ++= f.label.getOrElse("")
        b ++= discriminator(f.value)
      b.toString

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Arrow): String =
    val b = StringBuilder("X")
    for i <- t.inputs do
      b ++= i.label.getOrElse("")
      b ++= discriminator(i.value)
    b ++= discriminator(t.output)
    b.toString

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Sum): String =
    if t.members.isEmpty then "N" else
      "E" + t.members.map(discriminator).mkString

  /** Returns a transpiled reference to `e`. */
  private def transpiledReferenceTo(e: symbols.Entity): String =
    e match
      case symbols.Entity.Builtin(n, _) => s"alpine_rt.builtin.${n.identifier}"
      case symbols.Entity.Declaration(n, t) => scalaized(n) + discriminator(t)
      case _: symbols.Entity.Field => ???

  /** Returns a string representation of `n` suitable for use as a Scala identifier. */
  private def scalaized(n: symbols.Name): String =
    n.qualification match
      case Some(q) =>
        s"${scalaized(q)}_${n.identifier}"
      case None =>
        "_" + n.identifier

  override def visitLabeled[T <: ast.Tree](n: ast.Labeled[T])(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitBinding(n: ast.Binding)(using context: Context): Unit =
    // Bindings represent global symbols at top-level.
    if context.isTopLevel then
      context.output ++= "  " * context.indentation

      // If the is the entry point if it's called "main".
      if n.identifier == "main" then
        context.output ++= "@main def $entry"
      else
        context.output ++= s"private val "
        //context.output ++= transpiledReferenceTo(n.entityDeclared.)

      context.output ++= ": "
      context.output ++= transpiledType(n.tpe)

      // Top-level bindings must have an initializer.
      assert(n.initializer.isDefined)
      context.indentation += 1
      context.output ++= " =\n"
      context.output ++= "  " * context.indentation
      context.inScope((c) => n.initializer.get.visit(this)(using c))
      context.output ++= "\n\n"
      context.indentation -= 1

    // Bindings at local-scope are used in let-bindings and pattern cases.
    else
      context.output ++= s"val "
      context.output ++= transpiledReferenceTo(n.entityDeclared)
      context.output ++= ": "
      context.output ++= transpiledType(n.tpe)
      n.initializer.map { (i) =>
        context.output ++= " = "
        context.inScope((c) => i.visit(this)(using c))
      }

  override def visitTypeDeclaration(n: ast.TypeDeclaration)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitFunction(n: ast.Function)(using context: Context): Unit =
    context.output ++= "  " * context.indentation
    context.output ++= "def "
    context.output ++= transpiledReferenceTo(n.entityDeclared)
    context.output ++= "("
    context.output.appendCommaSeparated(n.inputs) { (o, a) =>
      o ++= a.identifier
      o ++= ": "
      o ++= transpiledType(a.tpe)
    }
    context.output ++= "): "
    context.output ++= transpiledType(symbols.Type.Arrow.from(n.tpe).get.output)

    context.indentation += 1
    context.output ++= "  " * context.indentation
    context.inScope((c) => n.body.visit(this)(using c))
    context.output ++= "\n\n"
    context.indentation -= 1

  override def visitParameter(n: ast.Parameter)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitIdentifier(n: ast.Identifier)(using context: Context): Unit =
    context.output ++= transpiledReferenceTo(n.referredEntity.get.entity)

  override def visitBooleanLiteral(n: ast.BooleanLiteral)(using context: Context): Unit =
    context.output ++= n.value.toString

  override def visitIntegerLiteral(n: ast.IntegerLiteral)(using context: Context): Unit =
    context.output ++= n.value.toString

  override def visitFloatLiteral(n: ast.FloatLiteral)(using context: Context): Unit =
    context.output ++= n.value.toString ++ "f"

  override def visitStringLiteral(n: ast.StringLiteral)(using context: Context): Unit =
    context.output ++= n.value

  override def visitRecord(n: ast.Record)(using context: Context): Unit =
    //ALM
    if (n.tpe == symbols.Type.Unit) then
      context.output ++= "()"
    else
      val t = symbols.Type.Record.from(n.tpe).get
      context.registerUse(t)
      context.output ++= discriminator(t)
      if n.fields.isEmpty then return
      context.output ++= "("
      context.output.appendCommaSeparated(n.fields) { (o, f) =>
        context.swappingOutputBuffer(o) { (c) =>
          f.value.visit(this)(using c)
        }
      }
      context.output ++= ")"

  override def visitSelection(n: ast.Selection)(using context: Context): Unit =
    n.qualification.visit(this)
    n.referredEntity match
      case Some(symbols.EntityReference(e: symbols.Entity.Field, _)) =>
        context.output ++= ".$" + e.index
      case _ =>
        unexpectedVisit(n.selectee)

  override def visitApplication(n: ast.Application)(using context: Context): Unit =
    n.function.visit(this)
    context.output ++= "("
    context.output.appendCommaSeparated(n.arguments) { (o, a) => a.value.visit(this) }
    context.output ++= ")"

  override def visitPrefixApplication(n: ast.PrefixApplication)(using context: Context): Unit =
    n.function.visit(this)
    context.output ++= "("
    n.argument.visit(this)
    context.output ++= ")"

  override def visitInfixApplication(n: ast.InfixApplication)(using context: Context): Unit =
    n.function.visit(this)
    context.output ++= "("
    n.lhs.visit(this)
    context.output ++= ", "
    n.rhs.visit(this)
    context.output ++= ")"

  override def visitConditional(n: ast.Conditional)(using context: Context): Unit =
    context.output ++= "if "
    n.condition.visit(this)
    context.output ++= " then "
    n.successCase.visit(this)
    context.output ++= " else "
    n.failureCase.visit(this)

  override def visitMatch(n: ast.Match)(using context: Context): Unit =
    //ALM: FINI
    context.output ++= "  " * context.indentation
    n.scrutinee.visit(this)
    context.output ++= " match"
    context.output ++= " {\n"
    context.indentation += 1
    n.cases.foreach(_.visit(this))
    context.indentation -= 1
    context.output ++= "  " * context.indentation
    context.output ++= "}"


  override def visitMatchCase(n: ast.Match.Case)(using context: Context): Unit =
    //ALM: FINI
    context.output ++= "\n"
    context.output ++= "  " * context.indentation
    context.output ++= "case "
    //if n.pattern represent a value, add "`" between the pattern 
    n.pattern match
      case ast.ValuePattern(value, site)  => 
        value match
          case ast.Identifier(value, site) =>
              context.output ++= "`"
              n.pattern.visit(this)
              context.output ++= "`"
                                          
          case _ => n.pattern.visit(this)
          
      case a@ ast.Binding(id, asc, init, site) => 
        context.output ++= transpiledReferenceTo(a.entityDeclared)
        asc match
          case Some(t) =>
            context.output ++= ": "
            context.output ++= transpiledType(t.tpe)
          case None => context.output ++= ""

      case _ => 
        n.pattern.visit(this)
      
      context.output ++= " => "
      n.body.visit(this)
    

  override def visitLet(n: ast.Let)(using context: Context): Unit =
    // Use a block to uphold lexical scoping.
    context.output ++= "{\n"
    context.indentation += 1
    context.output ++= "  " * context.indentation
    n.binding.visit(this)
    context.output ++= "\n"
    context.output ++= "  " * context.indentation
    n.body.visit(this)
    context.output ++= "\n"
    context.indentation -= 1
    context.output ++= "  " * context.indentation
    context.output ++= "}"

  override def visitLambda(n: ast.Lambda)(using context: Context): Unit =
    context.output ++= "("
    context.output.appendCommaSeparated(n.inputs) { (o, a) =>
      o ++= a.identifier
      o ++= ": "
      o ++= transpiledType(a.tpe)
    }
    context.output ++= ") => ("
    n.body.visit(this)
    context.output ++= "): "
    context.output ++= transpiledType(symbols.Type.Arrow.from(n.tpe).get.output)

  override def visitParenthesizedExpression(
      n: ast.ParenthesizedExpression
  )(using context: Context): Unit =
    context.output ++= "("
    n.inner.visit(this)
    context.output ++= ")"

  override def visitAscribedExpression(
      n: ast.AscribedExpression
  )(using context: Context): Unit =
    //ALM: FINI ?
    n.operation match
      case Typecast.Widen => 
        n.inner.visit(this)
      case Typecast.Narrow =>
        Type.Sum.from(n.tpe) match
        case Some(opt) =>
          context.output ++= "alpine_rt.narrow["
          context.output ++= transpiledType(n.ascription.tpe)
          context.output ++= ", "
          context.output ++= transpiledType(n.tpe)
          context.output ++= "]("
          n.inner.visit(this)
          context.output ++= ", "
          opt.members.foreach((m) => context.registerUse(m))
          val some = opt.members.filter(_ != Type.none).head
          context.output ++= discriminator(some)
          context.output ++= ".apply"
          context.output ++= ", "
          context.output ++= discriminator(Type.none)
          context.output ++= ")"
        case None =>
          throw Error(s"unexpected type '${n.tpe}'")
      case Typecast.NarrowUnconditionally =>
        context.output ++= "if("
        n.inner.visit(this)
        context.output ++= ".isInstanceOf["
        context.output ++= transpiledType(n.ascription.tpe)
        context.output ++= "]) then {"
        n.inner.visit(this)
        context.output ++= ".asInstanceOf["
        context.output ++= transpiledType(n.ascription.tpe)
        context.output ++= "]} else alpine_rt.panic "
    

  override def visitTypeIdentifier(n: ast.TypeIdentifier)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitRecordType(n: ast.RecordType)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitTypeApplication(n: ast.TypeApplication)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitArrow(n: ast.Arrow)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitSum(n: ast.Sum)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitParenthesizedType(n: ast.ParenthesizedType)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitValuePattern(n: ast.ValuePattern)(using context: Context): Unit =
    //ALM: FINI
    n.value.visit(this)

  override def visitRecordPattern(n: ast.RecordPattern)(using context: Context): Unit =
    //ALM: FINI
    context.output ++= discriminator(n.tpe)
    if !n.fields.isEmpty then
      context.output ++= "("
    context.output.appendCommaSeparated(n.fields) { (o, f) =>
      //swap buffer
      context.swappingOutputBuffer(o) { (c) =>
        f.value match
          case ast.ValuePattern(value, site)  => 
            if value.isInstanceOf[ast.Identifier] then
              context.output ++= "`"
              f.value.visit(this)(using c)
              context.output ++= "`"
            else 
              f.value.visit(this)(using c)
          case a@ Binding(identifier, ascription, initializer, site) => 
            context.output ++= transpiledReferenceTo(a.entityDeclared)
            ascription match
              case Some(t) =>
                context.output ++= ": "
                context.output ++= transpiledType(t.tpe)
              case None => context.output ++= ""
          case _ => 
            f.value.visit(this)(using c)
      }
    }
    if !n.fields.isEmpty then
      context.output ++= ")"

  override def visitWildcard(n: ast.Wildcard)(using context: Context): Unit =
    //ALM: FINI
    context.output ++= "_"

  override def visitError(n: ast.ErrorTree)(using context: Context): Unit =
    unexpectedVisit(n)

object ScalaPrinter:

  /** The local state of a transpilation to Scala.
   *
   *  @param indentation The current identation to add before newlines.
   */
  final class Context(var indentation: Int = 0):

    /** The types that must be emitted in the program. */
    private var _typesToEmit = mutable.Set[symbols.Type.Record]()

    /** The types that must be emitted in the program. */
    def typesToEmit: Set[symbols.Type.Record] = _typesToEmit.toSet

    /** The (partial) result of the transpilation. */
    private var _output = StringBuilder()

    /** The (partial) result of the transpilation. */
    def output: StringBuilder = _output

    /** `true` iff the transpiler is processing top-level symbols. */
    private var _isTopLevel = true

    /** `true` iff the transpiler is processing top-level symbols. */
    def isTopLevel: Boolean = _isTopLevel

    /** Adds `t` to the set of types that are used by the transpiled program. */
    def registerUse(t: symbols.Type.Record): Unit =
      if t != symbols.Type.Unit then _typesToEmit.add(t)

    /** Returns `action` applied on `this` where `output` has been exchanged with `o`. */
    def swappingOutputBuffer[R](o: StringBuilder)(action: Context => R): R =
      val old = _output
      _output = o
      try action(this) finally _output = old

    /** Returns `action` applied on `this` where `isTopLevel` is `false`. */
    def inScope[R](action: Context => R): R =
      var tl = _isTopLevel
      _isTopLevel = false
      try action(this) finally _isTopLevel = tl

  end Context

end ScalaPrinter

extension (self: StringBuilder) def appendCommaSeparated[T](ls: Seq[T])(
    reduce: (StringBuilder, T) => Unit
): Unit =
    var f = true
    for l <- ls do
      if f then f = false else self ++= ", "
      reduce(self, l)
```



#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:11)
	dotty.tools.dotc.core.TypeOps$.dominators$1(TypeOps.scala:248)
	dotty.tools.dotc.core.TypeOps$.approximateOr$1(TypeOps.scala:382)
	dotty.tools.dotc.core.TypeOps$.orDominator(TypeOps.scala:395)
	dotty.tools.dotc.core.Types$OrType.join(Types.scala:3435)
	dotty.tools.dotc.core.Types$Type.classSymbol(Types.scala:531)
	dotty.tools.dotc.typer.Applications.targetClass$1(Applications.scala:2277)
	dotty.tools.dotc.typer.Applications.harmonizeWith(Applications.scala:2284)
	dotty.tools.dotc.typer.Applications.harmonize(Applications.scala:2311)
	dotty.tools.dotc.typer.Applications.harmonize$(Applications.scala:352)
	dotty.tools.dotc.typer.Typer.harmonize(Typer.scala:116)
	dotty.tools.dotc.typer.Typer.$anonfun$33(Typer.scala:1808)
	dotty.tools.dotc.typer.Applications.harmonic(Applications.scala:2334)
	dotty.tools.dotc.typer.Applications.harmonic$(Applications.scala:352)
	dotty.tools.dotc.typer.Typer.harmonic(Typer.scala:116)
	dotty.tools.dotc.typer.Typer.typedMatchFinish(Typer.scala:1808)
	dotty.tools.dotc.typer.Typer.typedMatch(Typer.scala:1742)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3062)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3112)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3184)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3188)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3237)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3256)
	dotty.tools.dotc.typer.Typer.typedBlockStats(Typer.scala:1159)
	dotty.tools.dotc.typer.Typer.typedBlock(Typer.scala:1163)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3056)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3112)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3184)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3188)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3300)
	dotty.tools.dotc.typer.Typer.$anonfun$57(Typer.scala:2486)
	dotty.tools.dotc.inlines.PrepareInlineable$.dropInlineIfError(PrepareInlineable.scala:243)
	dotty.tools.dotc.typer.Typer.typedDefDef(Typer.scala:2486)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3024)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3111)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3184)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3188)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3210)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3256)
	dotty.tools.dotc.typer.Typer.typedClassDef(Typer.scala:2669)
	dotty.tools.dotc.typer.Typer.typedTypeOrClassDef$1(Typer.scala:3036)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3040)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3111)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3184)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3188)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3210)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3256)
	dotty.tools.dotc.typer.Typer.typedPackageDef(Typer.scala:2812)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3081)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3112)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3184)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3188)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3237)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3256)
	dotty.tools.dotc.typer.Typer.typedPackageDef(Typer.scala:2812)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3081)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3112)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3184)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3188)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3300)
	dotty.tools.dotc.typer.TyperPhase.typeCheck$$anonfun$1(TyperPhase.scala:44)
	dotty.tools.dotc.typer.TyperPhase.typeCheck$$anonfun$adapted$1(TyperPhase.scala:54)
	scala.Function0.apply$mcV$sp(Function0.scala:42)
	dotty.tools.dotc.core.Phases$Phase.monitor(Phases.scala:440)
	dotty.tools.dotc.typer.TyperPhase.typeCheck(TyperPhase.scala:54)
	dotty.tools.dotc.typer.TyperPhase.runOn$$anonfun$3(TyperPhase.scala:88)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.typer.TyperPhase.runOn(TyperPhase.scala:88)
	dotty.tools.dotc.Run.runPhases$1$$anonfun$1(Run.scala:246)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.ArrayOps$.foreach$extension(ArrayOps.scala:1321)
	dotty.tools.dotc.Run.runPhases$1(Run.scala:262)
	dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:270)
	dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:279)
	dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:67)
	dotty.tools.dotc.Run.compileUnits(Run.scala:279)
	dotty.tools.dotc.Run.compileSources(Run.scala:194)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:165)
	scala.meta.internal.pc.MetalsDriver.run(MetalsDriver.scala:45)
	scala.meta.internal.pc.PcCollector.<init>(PcCollector.scala:44)
	scala.meta.internal.pc.PcSemanticTokensProvider$Collector$.<init>(PcSemanticTokensProvider.scala:61)
	scala.meta.internal.pc.PcSemanticTokensProvider.Collector$lzyINIT1(PcSemanticTokensProvider.scala:61)
	scala.meta.internal.pc.PcSemanticTokensProvider.Collector(PcSemanticTokensProvider.scala:61)
	scala.meta.internal.pc.PcSemanticTokensProvider.provide(PcSemanticTokensProvider.scala:90)
	scala.meta.internal.pc.ScalaPresentationCompiler.semanticTokens$$anonfun$1(ScalaPresentationCompiler.scala:110)
```
#### Short summary: 

java.lang.AssertionError: assertion failed