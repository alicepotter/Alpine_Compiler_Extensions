file://<WORKSPACE>/src/main/scala/alpine/codegen/CPrinter.scala
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.1
Classpath:
<WORKSPACE>/.bloop/lab05_transpiler/bloop-bsp-clients-classes/classes-Metals-onmnaSl7RHe2p5nz_jJ9FQ== [exists ], <HOME>/Library/Caches/bloop/semanticdb/com.sourcegraph.semanticdb-javac.0.9.10/semanticdb-javac-0.9.10.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/sourcegraph/semanticdb-javac/0.9.10/semanticdb-javac-0.9.10.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.1/scala3-library_3-3.3.1.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.10/scala-library-2.13.10.jar [exists ]
Options:
-Xsemanticdb -sourceroot <WORKSPACE>


action parameters:
offset: 10222
uri: file://<WORKSPACE>/src/main/scala/alpine/codegen/CPrinter.scala
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
import scala.compiletime.ops.int
import alpine.ast.ErrorTree
import alpine.ast.Identifier
import alpine.ast.Literal
import alpine.ast.IntegerLiteral
import alpine.ast.Selection
import alpine.ast.Application
import alpine.ast.PrefixApplication
import alpine.ast.InfixApplication
import alpine.ast.Conditional
import alpine.ast.Match
import alpine.ast.Let
import alpine.ast.Lambda
import alpine.ast.ParenthesizedExpression
import alpine.ast.AscribedExpression
import alpine.ast.Match.Case

/** The transpilation of an Alpine program to Scala. */
final class CPrinter(syntax: TypedProgram) extends ast.TreeVisitor[CPrinter.Context, Unit]:

    import CPrinter.Context

    private given TypedProgram = syntax

    /** Returns a C program equivalent to syntax. */
    def transpile(): String =
        given c: Context = Context()
        emitBasicTypes
        syntax.declarations.foreach(_.visit(this))
        //c.typesToEmit.map(emitRecord)
        c.output.toString

    def emitBasicTypes(using context: Context): Unit =
        //Create enum containing ints representing types
        context.output ++= "typedef enum {"
        context.output ++= "IntType = 0, "
        context.output ++= "FloatType = 1, "
        context.output ++= "BooleanType = 2, "
        context.output ++= "StringType = 3, "
        context.output ++= "RecordType = 4, "
        context.output ++= "AnyType = 5"
        context.output ++= "} TypeEnum;\n"
        //Create struct representing types
        context.output ++= "typedef struct {TypeEnum type;} Type;\n"
        //Create struct representing any type
        context.output ++=  """typedef struct {void* payload; Type* type;} Any;
typedef struct {void* payload; Type* type;} Int;
typedef struct {void* payload; Type* type;} Float;
typedef struct {void* payload; Type* type;} Boolean;
typedef struct {void* payload; Type* type;} String;
typedef struct {void* fields; Type* type;} Record;""".stripMargin
        context.output ++= "\n"


    /** Makes an integer literal using the struct defined at the beginning of the program. */
    private def makeInt(value: String)(using Context): String =
        s"(Int) { (void*) $value, IntType }"

    
    /** Makes a boolean literal using the struct defined at the beginning of the program. */
    private def makeBoolLiteral(value: String)(using Context): String =
        if value == "true" then s"Boolean{.payload = (void*)1, .type = (Type*) BooleanType}"
        else s"(Boolean) {.payload = (void*)0, .type = (Type*) BooleanType}"
    
    /** Makes a float literal using the struct defined at the beginning of the program. */
    private def makeFloatLiteral(value: String)(using Context): String =
        s"(Float) { .payload = (void*) $value, .type = (Type*) FloatType }"

    /** Makes a string literal using the struct defined at the beginning of the program. */
    private def makeStringLiteral(value: String)(using Context): String =
        s"(String) { .payload = (void*) $value, .type = (Type*) StringType }"


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
        case _ => throw Error(s"type '${t}' is not representable in C")

    /** Returns the transpiled form of `t` in C. */
    private def transpiledBuiltin(t: symbols.Type.Builtin)(using context: Context): String =
        t match
        case symbols.Type.BuiltinModule => throw Error(s"type '${t}' is not representable in C")
        case symbols.Type.Bool => s"Boolean"
        case symbols.Type.Int => s"Int"
        case symbols.Type.Float => s"Float"
        case symbols.Type.String => s"String"
        case symbols.Type.Any => s"Any"

    /** Returns the transpiled form of `t`. */
    private def transpiledRecord(t: symbols.Type.Record)(using context: Context): String = 
        if t == symbols.Type.Unit then
            "Record{.fields = NULL, .type = (Type*) RecordType}"
        else
            val r = StringBuilder()
            r ++= "Record{.fields = (void*) malloc(sizeof(Record)), .type = (Type*) RecordType}"
            r ++= "{"
            for f <- t.fields do
                r ++= "."
                r ++= f.label.getOrElse("")
                r ++= " = "
                r ++= transpiledType(f.value)
                r ++= ", "
            r ++= "}"
            r.toString()


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
    private def transpiledSum(t: symbols.Type.Sum)(using context: Context): String = ???

    /** Returns a transpiled reference to `e`. */
    private def transpiledReferenceTo(e: symbols.Entity): String =
        e match
            case symbols.Entity.Builtin(n, _) => 
                if n.identifier == "print" then "printf" else s"alpine_rt.builtin.${n.identifier}"
            case symbols.Entity.Declaration(n, t) => scalaized(n) + discriminator(t)
            case _: symbols.Entity.Field => ???

    /** Returns a string representation of `n` suitable for use as a Scala identifier. */
    private def scalaized(n: symbols.Name): String =
        n.qualification match
            case Some(q) =>
                s"${scalaized(q)}_${n.identifier}"
            case None =>
                "_" + n.identifier

    override def visitConditional(n: ast.Conditional)(using context: Context): Unit =
        context.output ++= "if ("
        n.condition.visit(this)
        context.output ++= " )"
        n.successCase.visit(this)
        context.output ++= "} else {"
        n.failureCase.visit(this)
        context.output ++= "}"

    override def visitParenthesizedExpression(
        n: ast.ParenthesizedExpression
        )(using context: Context): Unit =
        context.output ++= "("
        n.inner.visit(this)
        context.output ++= ")"

    override def visitLet(n: ast.Let)(using context: Context): Unit =
        //ALAMAIN : NOTSURE
        context.output ++= transpiledReferenceTo(n.binding.entityDeclared)
        context.output ++= " "
        n.binding.visit(this)
        context.output ++= ";\n"
        context.output ++= "\n"
        n.body.visit(this)
        context.output ++= "\n"
        context.output ++= "}"

    override def visitBinding(n: ast.Binding)(using context: Context): Unit =
        if context.isTopLevel then
            if n.identifier == "main" then
                context.output ++= "int main() {\n"
                n.initializer.get.visit(this)(using context)
                context.output ++= ";\n"
                context.output ++= "return 0;\n"
                context.output ++= "}\n"
            else
                context.output ++= transpiledType(n.entityDeclared.tpe)
                context.output ++= " "
                context.output ++= transpiledReferenceTo(n.entityDeclared)
                context.output ++= " = "
                n.initializer.get.visit(this)(using context) //This does not work if it is of type Any
                context.output ++= ";\n"

        else
            n.initializer.get.visit(this)



    override def visitFunction(n: ast.Function)(using context: Context): Unit =
        //return type 
        context.output ++= transpiledType(symbols.Type.Arrow.from(n.tpe).get.output) //A prendre en compte : le type de retour void 

        //function name
        context.output ++= " "
        context.output ++= transpiledReferenceTo(n.entityDeclared)
        context.output ++= "("

        //parameters
        context.output.appendCommaSeparated(n.inputs) { (o, a) =>
            o ++= transpiledType(a.tpe)
            o ++= " "
            o ++= transpiledReferenceTo(a.entityDeclared)
        }
        context.output ++= ") {\n"

        //body
        context.output ++= "return " //Will only work with a function with one line of body
        context.inScope((c) => n.body.visit(this)(using c))
        context.output ++= ";"
        context.output ++= "\n}"

    //NO SURE IS NECESSARY
    private def transpiledBuiltinTypeEnum(t: symbols.Type)(using context: Context): String =
        t match
        case symbols.Type.BuiltinModule => throw Error(s"type '${t}' is not representable in C")
        case symbols.Type.Bool => s"BooleanType"
        case symbols.Type.Int => s"IntType"
        case symbols.Type.Float => s"FloatType"
        case symbols.Type.String => s"StringType"
        case symbols.Type.Any => s"AnyType"

    override def visitMatch(n: ast.Match)(using context: Context): Unit =

        def helper(c : List[Case]): Unit =
            if (c.size == 1) {
                c.head.body.visit(this)
            } else {
                context.output ++= "("
                context.output ++= transpiledReferenceTo(n.scrutinee.referredEntity.get.entity)
                context.output ++= ".type == "
                context.output ++= transpiledBuiltinTypeEnum(c.head.pattern.tpe) //NOT SURE IT WORKS FOR ALL CASES
                context.output ++= " ? "
                c.head.body.visit(this)
                context.output ++= " : "
                helper(c.tail)
                context.output ++= " )"
            }

        helper(n.cases)

    def printfArgument(n: Labeled[@@])

    def visitApplication (n: alpine.ast.Application)(using a: alpine.codegen.CPrinter.Context): Unit = 
        n.function match
            case Identifier(value, _) =>
                if value == "print" then
                    a.output ++= "printf("
                    //Check the type of the argument and print it accordingly
                    //Suppose there is only one argument

                    a.output ++= ")"
                else
                    n.function.visit(this)
                    a.output ++= "("
                    a.output.appendCommaSeparated(n.arguments) { (o, a) => a.value.visit(this) }
                    a.output ++= ")"
            case _ => 
                n.function.visit(this)
                a.output ++= "("
                a.output.appendCommaSeparated(n.arguments) { (o, a) => a.value.visit(this) }
                a.output ++= ")"
        
      

    def visitArrow (n: alpine.ast.Arrow)(using a: alpine.codegen.CPrinter.Context): Unit = ???
    def visitAscribedExpression(n: alpine.ast.AscribedExpression)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitBooleanLiteral(n: alpine.ast.BooleanLiteral)(using a: alpine.codegen.CPrinter.Context): Unit = 
        a.output ++= makeBoolLiteral(n.value)

    def visitError(n: alpine.ast.ErrorTree)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitFloatLiteral(n: alpine.ast.FloatLiteral)(using a: alpine.codegen.CPrinter.Context): Unit = 
        a.output ++= makeFloatLiteral(n.value)

    def visitIdentifier(n: alpine.ast.Identifier)(using a: alpine.codegen.CPrinter.Context): Unit = 
        a.output ++= transpiledReferenceTo(n.referredEntity.get.entity)

    def visitInfixApplication(n: alpine.ast.InfixApplication)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitIntegerLiteral(n: alpine.ast.IntegerLiteral)(using a: alpine.codegen.CPrinter.Context): Unit = 
        a.output ++= makeInt(n.value)

    def visitLabeled[T <: alpine.ast.Tree](n: alpine.ast.Labeled[T])(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitLambda(n: alpine.ast.Lambda)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitMatchCase (n: alpine.ast.Match.Case)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitParameter (n: alpine.ast.Parameter)(using a: alpine.codegen.CPrinter.Context): Unit = 
        unexpectedVisit(n)

    def visitParenthesizedType(n: alpine.ast.ParenthesizedType)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitPrefixApplication(n: alpine.ast.PrefixApplication)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitRecord(n: alpine.ast.Record)(using a: alpine.codegen.CPrinter.Context): Unit = 
        //Add to the top with all the types 
        val t = symbols.Type.Record.from(n.tpe).get
        a.registerUse(t) // TO CHANGE TOMORROW
        //a.output ++= discriminator(t)
        //TO FINISH

    def visitRecordPattern (n: alpine.ast.RecordPattern)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitRecordType(n: alpine.ast.RecordType)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitSelection(n: alpine.ast.Selection)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitStringLiteral (n: alpine.ast.StringLiteral)(using a: alpine.codegen.CPrinter.Context): Unit = 
        a.output ++= makeStringLiteral(n.value)

    def visitSum(n: alpine.ast.Sum)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitTypeApplication (n: alpine.ast.TypeApplication) (using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitTypeDeclaration (n: alpine.ast.TypeDeclaration) (using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitTypeIdentifier (n: alpine.ast.TypeIdentifier)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitValuePattern (n: alpine.ast.ValuePattern)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitWildcard (n: alpine.ast.Wildcard)(using a: alpine.codegen.CPrinter.Context): Unit = 
       ???



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


object CPrinter:

    final class Context():

        /** The types that must be emitted in the program. */
        private var _typesToEmit = mutable.Set[symbols.Type.Record]()

        /** The types that must be emitted in the program. */
        def typesToEmit: Set[symbols.Type.Record] = _typesToEmit.toSet

        /** The (partial) result of the transpilation. */
        private var _output = StringBuilder()

        def output: StringBuilder = _output

        /** true iff the transpiler is processing top-level symbols. */
        private var _isTopLevel = true

        /** true iff the transpiler is processing top-level symbols. */
        def isTopLevel: Boolean = _isTopLevel

        /** Adds t to the set of types that are used by the transpiled program. */
        def registerUse(t: symbols.Type.Record): Unit =
            if t != symbols.Type.Unit then 
                //CREATE A STRUCT TO ADD TO THE TOP
                _typesToEmit.add(t)

        def inScope[R](action: Context => R): R =
            var tl = _isTopLevel
            _isTopLevel = false
            try action(this) finally _isTopLevel = tl



    end Context

end CPrinter
```



#### Error stacktrace:

```
dotty.tools.dotc.core.SymDenotations$NoDenotation$.owner(SymDenotations.scala:2582)
	scala.meta.internal.pc.SignatureHelpProvider$.isValid(SignatureHelpProvider.scala:83)
	scala.meta.internal.pc.SignatureHelpProvider$.notCurrentApply(SignatureHelpProvider.scala:94)
	scala.meta.internal.pc.SignatureHelpProvider$.$anonfun$1(SignatureHelpProvider.scala:48)
	scala.collection.StrictOptimizedLinearSeqOps.loop$3(LinearSeq.scala:280)
	scala.collection.StrictOptimizedLinearSeqOps.dropWhile(LinearSeq.scala:282)
	scala.collection.StrictOptimizedLinearSeqOps.dropWhile$(LinearSeq.scala:278)
	scala.collection.immutable.List.dropWhile(List.scala:79)
	scala.meta.internal.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:48)
	scala.meta.internal.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:412)
```
#### Short summary: 

java.lang.AssertionError: NoDenotation.owner