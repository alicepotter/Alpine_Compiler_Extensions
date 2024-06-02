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
import alpine.ast.Expression
import scala.annotation.meta.field
import alpine.ast.ValuePattern
import alpine.ast.RecordPattern
import alpine.ast.Wildcard
import scala.compiletime.ops.double
import alpine.ast.TypeIdentifier
import alpine.ast.RecordType
import alpine.ast.TypeApplication
import alpine.ast.Arrow
import alpine.ast.Sum
import alpine.ast.ParenthesizedType
import scala.runtime.stdLibPatches.language.deprecated.symbolLiterals

/** The transpilation of an Alpine program to Scala. */
final class CPrinter(syntax: TypedProgram) extends ast.TreeVisitor[CPrinter.Context, Unit]:

    import CPrinter.Context

    private given TypedProgram = syntax

    /** Returns a C program equivalent to syntax. */
    def transpile(): String =
        given c: Context = Context()
        emitBasicTypes
        syntax.declarations.foreach(_.visit(this))
        c.initialOutput.toString + c.globalVariables.toString + "\nvoid initializeGlobals() {\n" +
          "\n" + c.initializeGlobals.toString + "\n}\n\n" + c.functionOutput.toString + "\nint main() {\n" + c.output.toString + "\nreturn 0;\n}\n"

    def emitBasicTypes(using context: Context): Unit = 
        //Create enum containing ints representing types
        context.initialOutput ++= "\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>"
        context.initialOutput ++= "\n\ntypedef enum {"
        context.initialOutput ++= "IntType = 0, "
        context.initialOutput ++= "FloatType = 1, "
        context.initialOutput ++= "BooleanType = 2, "
        context.initialOutput ++= "StringType = 3, "
        context.initialOutput ++= "RecordType = 4, "
        context.initialOutput ++= "AnyType = 5"
        context.initialOutput ++= "} TypeEnum;\n\n"
        //Create struct representing types
        //context.initialOutput ++= "typedef struct {TypeEnum type;} Type;\n\n"
        //Create struct representing any type
        context.initialOutput ++=  """typedef struct {void* payload; TypeEnum type;} Any;
                            |typedef struct {void* payload; TypeEnum type;} Int;
                            |typedef struct {void* payload; TypeEnum type;} Float;
                            |typedef struct {void* payload; TypeEnum type;} Boolean;
                            |typedef struct {void* payload; TypeEnum type;} String;
                            |typedef struct {size_t nb_fields; void** fields; TypeEnum type; char* name;} Record;""".stripMargin
        context.initialOutput ++= "\n"
        context.initialOutput ++= "\nRecord* createRecord(size_t nb_fields, void** fields, char* name) {\n"
        context.initialOutput ++= "    Record* r = malloc(sizeof(Record));\n"
        context.initialOutput ++= "    r->nb_fields = nb_fields;\n"
        context.initialOutput ++= "    r->fields = fields;\n"
        context.initialOutput ++= "    r->type = RecordType;\n"
        context.initialOutput ++= "    r->name = name;\n"
        context.initialOutput ++= "    return r;\n"
        context.initialOutput ++= "}\n"
        context.initialOutput ++= "\nInt add(Int x, Int y) {\n"
        context.initialOutput ++= "    return (Int) {.payload = (void*) ((int)x.payload + (int)y.payload), .type = IntType };\n"
        context.initialOutput ++= "}\n"
        context.initialOutput ++= "Any* typeToAny(void* payload, TypeEnum type) {\n"
        context.initialOutput ++= "    Any* a = malloc(sizeof(Any));\n"
        context.initialOutput ++= "    a->payload = payload;\n"
        context.initialOutput ++= "    a->type = type;\n"
        context.initialOutput ++= "    return a;\n"
        context.initialOutput ++= "}\n\n"
        context.initialOutput ++= "Float* intToFloat(void* payload) {\n"
        context.initialOutput ++= "    Float* a = malloc(sizeof(Float));\n"
        context.initialOutput ++= "    a->payload = payload;\n"
        context.initialOutput ++= "    a->type = FloatType;\n"
        context.initialOutput ++= "    return a;\n"
        context.initialOutput ++= "}\n\n"
        context.initialOutput ++= "Int* anyToInt(void* payload) {\n"
        context.initialOutput ++= "    Int* a = malloc(sizeof(Int));\n"
        context.initialOutput ++= "    a->payload = payload;\n"
        context.initialOutput ++= "    a->type = IntType;\n"
        context.initialOutput ++= "    return a;\n"
        context.initialOutput ++= "}\n\n"
        context.initialOutput ++= "Float* anyToFloat(void* payload) {\n"
        context.initialOutput ++= "    Float* a = malloc(sizeof(Float));\n"
        context.initialOutput ++= "    a->payload = payload;\n"
        context.initialOutput ++= "    a->type = FloatType;\n"
        context.initialOutput ++= "    return a;\n"
        context.initialOutput ++= "}\n\n"
        context.initialOutput ++= "Boolean* anyToBool(void* payload) {\n"
        context.initialOutput ++= "    Boolean* a = malloc(sizeof(Boolean));\n"
        context.initialOutput ++= "    a->payload = payload;\n"
        context.initialOutput ++= "    a->type = BooleanType;\n"
        context.initialOutput ++= "    return a;\n"
        context.initialOutput ++= "}\n\n"
        context.initialOutput ++= "String* anyToString(void* payload) {\n"
        context.initialOutput ++= "    String* a = malloc(sizeof(String));\n"
        context.initialOutput ++= "    a->payload = payload;\n"
        context.initialOutput ++= "    a->type = StringType;\n"
        context.initialOutput ++= "    return a;\n"
        context.initialOutput ++= "}\n\n"
        context.initialOutput ++= "Any ascribed_inner;\n"
        context.initialOutput ++= "Boolean ascribed_inner_b;\n"
        context.initialOutput ++= "Int ascribed_inner_i;\n"
        context.initialOutput ++= "Float ascribed_inner_f;\n"
        context.initialOutput ++= "String ascribed_inner_s;\n"


        /** Makes an integer literal using the struct defined at the beginning of the program. */
    private def makeInt(value: String)(using context: Context): String =
        if (context.isAny)
            context.isAny = false
            s"(Any) {.payload = (void*) $value, .type = IntType }"
        else 
            s"(Int) {.payload = (void*) $value, .type = IntType }"

    /** Makes a boolean literal using the struct defined at the beginning of the program. */
    private def makeBoolLiteral(value: String)(using context: Context): String =
        if (context.isAny)
            if (value == "false") 
                context.isAny = false
                s"(Any) {.payload = (void*)0, .type = BooleanType}"
            else 
                context.isAny = false
                s"(Any) {.payload = (void*)1, .type = BooleanType}"
        else if (value == "true") 
            s"(Boolean) {.payload = (void*)1, .type = BooleanType}"
        else 
            s"(Boolean) {.payload = (void*)0, .type = BooleanType}"

    /** Makes a float literal using the struct defined at the beginning of the program. */
    private def makeFloatLiteral(value: String)(using context: Context): String =
            if (context.isAny)
                s"(Any) { .payload = (void*) $value, .type = FloatType }"
            else 
                s"(Float) { .payload = (void*) $value, .type = FloatType }"

    /** Makes a string literal using the struct defined at the beginning of the program. */
    private def makeStringLiteral(value: String)(using context: Context): String =
        if (context.isAny)
            context.isAny = false
            s"(Any) { .payload = (void*) $value, .type = StringType }"
        else 
            s"(String) { .payload = (void*) $value, .type = StringType }"

    /** Returns the transpiled form of t. */
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

    /** Returns the transpiled form of t in C. */
    private def transpiledBuiltin(t: symbols.Type.Builtin)(using context: Context): String =
        t match
        case symbols.Type.BuiltinModule => throw Error(s"type '${t}' is not representable in C")
        case symbols.Type.Bool => s"Boolean"
        case symbols.Type.Int => s"Int"
        case symbols.Type.Float => s"Float"
        case symbols.Type.String => s"String"
        case symbols.Type.Any => s"Any"

    /** Returns the transpiled form of t. */
    private def transpiledRecord(t: symbols.Type.Record)(using context: Context): String = 
        //CALLED BY VISIT BINDING    
        val r = StringBuilder()
        r ++= "Record"
        r.toString()


    /** Returns the transpiled form of t. */
    private def transpiledArrow(t: symbols.Type.Arrow)(using context: Context): String =
        val r = StringBuilder()
        r ++= "("
        r.appendCommaSeparated(t.inputs) { (o, a) => o ++= transpiledType(a.value) }
        r ++= " => "
        r ++= transpiledType(t.output)
        r ++= ")"
        r.toString()

    /** Returns the transpiled form of t. */
    private def transpiledSum(t: symbols.Type.Sum)(using context: Context): String = ???

    /** Returns a transpiled reference to e. */
    private def transpiledReferenceTo(e: symbols.Entity): String =
        e match
            case symbols.Entity.Builtin(n, _) => 
                if n.identifier == "print" then "printf" 
                else if n.identifier == "iadd" then "add"
                else s"alpine_rt.builtin.${n.identifier}"
            case symbols.Entity.Declaration(n, t) => scalaized(n) + discriminator(t)
            case _: symbols.Entity.Field => ???

    /** Returns a string representation of n suitable for use as a Scala identifier. */
    private def scalaized(n: symbols.Name): String =
        n.qualification match
            case Some(q) =>
                s"${scalaized(q)}_${n.identifier}"
            case None =>
                "_" + n.identifier

    override def visitConditional(n: ast.Conditional)(using context: Context): Unit =
        if (context.isInFunction) {
            context.functionOutput ++= "if ("
            n.condition.visit(this)
            context.functionOutput ++= " )"
            n.successCase.visit(this)
            context.functionOutput ++= "} else {"
            n.failureCase.visit(this)
            context.functionOutput ++= "}"
        } else {
            context.output ++= "if ("
            n.condition.visit(this)
            context.output ++= " )"
            n.successCase.visit(this)
            context.output ++= " else "
            n.failureCase.visit(this)
        }   

    def myParenthesizedExpression(n: ast.ParenthesizedExpression, name: String 
        )(using context: Context): Unit =
        if (n.inner.toString().take(18) == "AscribedExpression") {
            n.inner.visit(this)
            }
        else {
            if (context.inMain) {
                context.output ++= "("
                n.inner.visit(this)
                context.output ++= ")"
            } else if (context.isInFunction) {    
                context.functionOutput ++= "("
                n.inner.visit(this)
                context.functionOutput ++= ")"
            } else {
                context.initializeGlobals ++= "("
                n.inner.visit(this)
                context.initializeGlobals ++= ")"
            }
            }

    override def visitParenthesizedExpression(
        n: ast.ParenthesizedExpression
        )(using context: Context): Unit =
            if (n.inner.toString().take(18) == "AscribedExpression") {
                n.inner.visit(this)
            } else {
                if (context.inMain) {
                    context.output ++= "("
                    n.inner.visit(this)
                    context.output ++= ")"
                } else if (context.isInFunction) {    
                    context.functionOutput ++= "("
                    n.inner.visit(this)
                    context.functionOutput ++= ")"
                } else {
                    context.initializeGlobals ++= "("
                    n.inner.visit(this)
                    context.initializeGlobals ++= ")"
                }
            }

    override def visitLet(n: ast.Let)(using context: Context): Unit =

        if (context.isInFunction) {

            context.functionOutput ++= transpiledReferenceTo(n.binding.entityDeclared)
            context.functionOutput ++= " "
            n.binding.visit(this)
            context.functionOutput ++= ";\n"
            context.functionOutput ++= "\n"
            n.body.visit(this)
            context.functionOutput ++= "\n"
            context.functionOutput ++= "}"

        } else if (context.inMain){

            context.output ++= transpiledReferenceTo(n.binding.entityDeclared)
            context.output ++= " "
            n.binding.visit(this)
            context.output ++= ";\n"
            context.output ++= "\n"
            n.body.visit(this)
            context.output ++= "\n"
            context.output ++= "}"

        } else {

            //We are in the global variables initializer 

            context.initializeGlobals ++= transpiledReferenceTo(n.binding.entityDeclared)
            context.initializeGlobals ++= " "
            n.binding.visit(this)
            context.initializeGlobals ++= ";\n"
            context.initializeGlobals ++= "\n"
            n.body.visit(this)
            context.initializeGlobals ++= "\n"
            context.initializeGlobals ++= "}"

        }

    override def visitBinding(n: ast.Binding)(using context: Context): Unit =
        if (context.isInFunction == false) {

            if context.isTopLevel then
                if n.identifier == "main" then
                    //context.output ++= "int main() {\n\n"
                    context.inMain = true
                    context.output ++= "initializeGlobals();\n"
                    n.initializer.get.visit(this)(using context)
                    //context.output ++= ";\n\n"
                    //context.output ++= "return 0;\n"
                    context.inMain = false
                    //context.output ++= "}\n"
                else
                   
                    if (n.initializer.get.toString().take(18) == "AscribedExpression")
                        myVisitAscribed(n.initializer.get.asInstanceOf[alpine.ast.AscribedExpression], transpiledReferenceTo(n.entityDeclared))
                    else if (transpiledType(n.entityDeclared.tpe) == "Record") then
                        //Probleme ici : on ne peut pas initialiser un record avec un record
                        myVisitRecord(n.initializer.get.asInstanceOf[ast.Record], transpiledReferenceTo(n.entityDeclared))
                    else if (n.initializer.get.toString().take(23) == "ParenthesizedExpression") then
                         myParenthesizedExpression(n.initializer.get.asInstanceOf[ast.ParenthesizedExpression], transpiledReferenceTo(n.entityDeclared))
                    else 
                        //Need to add the variable to the global variables
                        context.globalVariables ++= transpiledType(n.entityDeclared.tpe) + " "
                        context.globalVariables ++= transpiledReferenceTo(n.entityDeclared) + ";\n"

                        //Need to add the initialization of the variable to the initializeGlobals
                        context.initializeGlobals ++= transpiledReferenceTo(n.entityDeclared) + " = "
                        context.isAny =  n.entityDeclared.tpe == symbols.Type.Any
                        n.initializer.get.visit(this)(using context)
                        context.initializeGlobals ++= ";\n"
            else
                n.initializer.get.visit(this)

        } else {
            if context.isTopLevel then
                if (transpiledType(n.entityDeclared.tpe) == "Record") then
                    myVisitRecord(n.initializer.get.asInstanceOf[ast.Record], transpiledReferenceTo(n.entityDeclared))
                else 
                    context.functionOutput ++= transpiledType(n.entityDeclared.tpe)
                    context.functionOutput ++= " "
                    context.functionOutput ++= transpiledReferenceTo(n.entityDeclared)
                    context.functionOutput ++= " = "
                    context.isAny =  n.entityDeclared.tpe == symbols.Type.Any
                    n.initializer.get.visit(this)(using context)
                    context.functionOutput ++= ";\n"
            else
                n.initializer.get.visit(this)
        }

    override def visitFunction(n: ast.Function)(using context: Context): Unit =
        //return type 
        context.isInFunction = true
        context.functionOutput ++= transpiledType(symbols.Type.Arrow.from(n.tpe).get.output) //A prendre en compte : le type de retour void 

        //function name
        context.functionOutput ++= " "
        context.functionOutput ++= transpiledReferenceTo(n.entityDeclared)
        context.functionOutput ++= "("

        //parameters
        context.functionOutput.appendCommaSeparated(n.inputs) { (o, a) =>
            o ++= transpiledType(a.tpe)
            o ++= " "
            o ++= transpiledReferenceTo(a.entityDeclared)
        }
        context.functionOutput ++= ") {\n"

        //body
        context.functionOutput ++= "return " //Will only work with a function with one line of body
        context.inScope((c) => n.body.visit(this)(using c))
        context.functionOutput ++= ";"
        context.functionOutput ++= "\n}"
        context.isInFunction = false

    private def transpiledBuiltinTypeEnum(t: symbols.Type)(using context: Context): String =
        t match
        case symbols.Type.BuiltinModule => throw Error(s"type '${t}' is not representable in C")
        case symbols.Type.Bool => s"BooleanType"
        case symbols.Type.Int => s"IntType"
        case symbols.Type.Float => s"FloatType"
        case symbols.Type.String => s"StringType"
        case symbols.Type.Any => s"AnyType"
        case _ => s"RecordType"

    override def visitMatch(n: ast.Match)(using context: Context): Unit =
        var isFirst = true
        def helper(c : List[Case]): Unit =
            if (c.size == 1) {
                c.head.body.visit(this)
            } else {

                if (context.inMain) {
                    context.output ++= "\n"
                    c.head.pattern match
                        case ErrorTree(site) => 
                            //should not happen
                        case Binding(identifier, ascription, initializer, site) =>
                            context.output ++= "("
                            context.output ++= transpiledReferenceTo(n.scrutinee.referredEntity.get.entity)
                            context.output ++= ".type == "
                            context.output ++= transpiledBuiltinTypeEnum(c.head.pattern.tpe) //NOT SURE IT WORKS FOR ALL CASES
                            context.output ++= " ? "
                            c.head.body.visit(this)
                            context.output ++= " : "
                            helper(c.tail)
                            context.output ++= " )"

                        case ValuePattern(value, site) =>
                            context.output ++= "(("
                            value.visit(this)
                            context.output ++= ".payload == "
                            // if the referred entity is empty, then it is a value literal
                            if n.scrutinee.referredEntity.isEmpty then
                                n.scrutinee.visit(this)
                            else
                                context.output ++= transpiledReferenceTo(n.scrutinee.referredEntity.get.entity)
                            context.output ++= ".payload)"
                            context.output ++= " ? "
                            c.head.body.visit(this)
                            context.output ++= " : "
                            helper(c.tail)
                            context.output ++= " )"

                        case RecordPattern(identifier, fields, site) =>
                            //name of the record
                            context.output ++= "((strcmp("
                            if n.scrutinee.referredEntity.isEmpty then
                                if isFirst then 
                                    context.globalVariables ++= "Record temp_;\n"
                                    context.initializeGlobals.insert(0, myVisitRecordAsString(n.scrutinee.asInstanceOf[ast.Record], "temp_"))
                                    context.output ++= "temp_"
                                    isFirst = false
                                else
                                    context.output ++= "temp_"
                            else
                                context.output ++= transpiledReferenceTo(n.scrutinee.referredEntity.get.entity)
                            context.output ++= ".name, "
                            context.output ++= "\""
                            context.output ++= identifier.drop(1) //Remove the #
                            context.output ++= "_"
                            for f <- fields do
                                context.output ++= discriminator(f.value.tpe)
                            context.output ++= "\""
                            context.output ++= ") == 0)"
                            //context.output ++= transpiledReferenceTo(c.head.pattern.tpe)
                            //context.output ++= transpiledBuiltinTypeEnum(c.head.pattern.tpe) //NOT SURE IT WORKS FOR ALL CASES
                            context.output ++= " && "
                            var i = 0
                            fields.foreach(f => 
                              context.output ++= "("
                              f.value.visit(this)
                                context.output ++= ".payload == "  
                                //cast to the right type
                                context.output ++= "(("
                                context.output ++= transpiledType(f.value.tpe)
                                context.output ++= "*)"
                                if n.scrutinee.referredEntity.isEmpty then
                                    context.output ++= "temp_"
                                else
                                    context.output ++= transpiledReferenceTo(n.scrutinee.referredEntity.get.entity)
                                context.output ++= ".fields["
                                context.output ++= i.toString()
                                context.output ++= "])->payload"
                                i+=1
                                context.output ++= ")"
                                if(i < fields.size){
                                    context.output ++= " && "
                                }
                            )
                            context.output ++= " ? "
                            c.head.body.visit(this)
                            context.output ++= " : "
                            helper(c.tail)
                            context.output ++= " )"
                        case Wildcard(site) =>
                } else {
                if (context.isInFunction == false) {
                    context.initializeGlobals ++= "\n"
                    //If it is a record, we need to compare the record name not the type
                    
                    c.head.pattern match
                        case ErrorTree(site) => 
                        case Binding(identifier, ascription, initializer, site) =>
                            context.initializeGlobals ++= "("
                            context.initializeGlobals ++= transpiledReferenceTo(n.scrutinee.referredEntity.get.entity)
                            context.initializeGlobals ++= ".type == "
                            context.initializeGlobals ++= transpiledBuiltinTypeEnum(c.head.pattern.tpe) //NOT SURE IT WORKS FOR ALL CASES
                            context.initializeGlobals ++= " ? "
                            c.head.body.visit(this)
                            context.initializeGlobals ++= " : "
                            helper(c.tail)
                            context.initializeGlobals ++= " )"
                        case ValuePattern(value, site) =>

                            context.initializeGlobals ++= "(("
                            value.visit(this)
                            context.initializeGlobals ++= ".payload == "
                            // if the referred entity is empty, then it is a value literal
                            if n.scrutinee.referredEntity.isEmpty then
                                n.scrutinee.visit(this)
                            else
                                context.initializeGlobals ++= transpiledReferenceTo(n.scrutinee.referredEntity.get.entity)
                            context.initializeGlobals ++= ".payload)"
                            context.initializeGlobals ++= " ? "
                            c.head.body.visit(this)
                            context.initializeGlobals ++= " : "
                            helper(c.tail)
                            context.initializeGlobals ++= " )"

                        case RecordPattern(identifier, fields, site) =>
                            //name of the record
                            context.initializeGlobals ++= "((strcmp("
                            //if there is no referred entity then the record is a record literal and we visit it
                            if n.scrutinee.referredEntity.isEmpty then
                                if isFirst then 
                                    context.initializeGlobals.insert(0, myVisitRecordAsString(n.scrutinee.asInstanceOf[ast.Record], "temp_"))
                                    context.initializeGlobals ++= "temp_"
                                    isFirst = false
                                else
                                    context.initializeGlobals ++= "temp_"
                            else
                                context.initializeGlobals ++= transpiledReferenceTo(n.scrutinee.referredEntity.get.entity)
                            context.initializeGlobals ++= ".name, "
                            context.initializeGlobals ++= "\""
                            context.initializeGlobals ++= identifier.drop(1) //Remove the #
                            context.initializeGlobals ++= "_"
                            for f <- fields do
                                context.initializeGlobals ++= discriminator(f.value.tpe)
                            context.initializeGlobals ++= "\""
                            context.initializeGlobals ++= ") == 0)"
                            //context.output ++= transpiledReferenceTo(c.head.pattern.tpe)
                            //context.output ++= transpiledBuiltinTypeEnum(c.head.pattern.tpe) //NOT SURE IT WORKS FOR ALL CASES
                            context.initializeGlobals ++= " && "
                            var i = 0
                            fields.foreach(f => 
                              context.initializeGlobals ++= "("
                              f.value.visit(this)
                                context.initializeGlobals ++= ".payload == "  
                                //cast to the right type
                                context.initializeGlobals ++= "(("
                                context.initializeGlobals ++= transpiledType(f.value.tpe)
                                context.initializeGlobals ++= "*)"
                                if n.scrutinee.referredEntity.isEmpty then
                                    context.initializeGlobals ++= "temp_"
                                else
                                    context.initializeGlobals ++= transpiledReferenceTo(n.scrutinee.referredEntity.get.entity)
                                context.initializeGlobals ++= ".fields["
                                context.initializeGlobals ++= i.toString()
                                context.initializeGlobals ++= "])->payload"
                                i+=1
                                context.initializeGlobals ++= ")"
                                if(i < fields.size){
                                    context.initializeGlobals ++= " && "
                                }
                            )
                            context.initializeGlobals ++= " ? "
                            c.head.body.visit(this)
                            context.initializeGlobals ++= " : "
                            helper(c.tail)
                            context.initializeGlobals ++= " )"
                        case Wildcard(site) =>

                }                 
                else {
                    context.functionOutput ++= "\n"
                    
                    c.head.pattern match
                        case ErrorTree(site) => 
                        case Binding(identifier, ascription, initializer, site) =>
                            context.functionOutput ++= "("
                            context.functionOutput ++= transpiledReferenceTo(n.scrutinee.referredEntity.get.entity)
                            context.functionOutput ++= ".type == "
                            context.functionOutput ++= transpiledBuiltinTypeEnum(c.head.pattern.tpe) //NOT SURE IT WORKS FOR ALL CASES
                            context.functionOutput ++= " ? "
                            c.head.body.visit(this)
                            context.functionOutput ++= " : "
                            helper(c.tail)
                            context.functionOutput ++= " )"
                        case ValuePattern(value, site) =>

                            context.functionOutput ++= "(("
                            value.visit(this)
                            context.functionOutput ++= ".payload == "
                            // if the referred entity is empty, then it is a value literal
                            if n.scrutinee.referredEntity.isEmpty then
                                n.scrutinee.visit(this)
                            else
                                context.functionOutput ++= transpiledReferenceTo(n.scrutinee.referredEntity.get.entity)
                            context.functionOutput ++= ".payload)"
                            context.functionOutput ++= " ? "
                            c.head.body.visit(this)
                            context.functionOutput ++= " : "
                            helper(c.tail)
                            context.functionOutput ++= " )"

                        case RecordPattern(identifier, fields, site) =>
                            //name of the record
                            context.functionOutput ++= "((strcmp("
                            //if there is no referred entity then the record is a record literal and we visit it
                            if n.scrutinee.referredEntity.isEmpty then
                                if isFirst then 
                                    context.functionOutput.insert(0, myVisitRecordAsString(n.scrutinee.asInstanceOf[ast.Record], "temp_"))
                                    context.functionOutput ++= "temp_"
                                    isFirst = false
                                else
                                    context.functionOutput ++= "temp_"
                            else
                                context.functionOutput ++= transpiledReferenceTo(n.scrutinee.referredEntity.get.entity)
                            context.functionOutput ++= ".name, "
                            context.functionOutput ++= "\""
                            context.functionOutput ++= identifier.drop(1) //Remove the #
                            context.functionOutput ++= "_"
                            for f <- fields do
                                context.functionOutput ++= discriminator(f.value.tpe)
                            context.functionOutput ++= "\""
                            context.functionOutput ++= ") == 0)"
                            //context.output ++= transpiledReferenceTo(c.head.pattern.tpe)
                            //context.output ++= transpiledBuiltinTypeEnum(c.head.pattern.tpe) //NOT SURE IT WORKS FOR ALL CASES
                            context.functionOutput ++= " && "
                            var i = 0
                            fields.foreach(f => 
                              context.functionOutput ++= "("
                              f.value.visit(this)
                                context.functionOutput ++= ".payload == "  
                                //cast to the right type
                                context.functionOutput ++= "(("
                                context.functionOutput ++= transpiledType(f.value.tpe)
                                context.functionOutput ++= "*)"
                                if n.scrutinee.referredEntity.isEmpty then
                                    context.functionOutput ++= "temp_"
                                else
                                    context.functionOutput ++= transpiledReferenceTo(n.scrutinee.referredEntity.get.entity)
                                context.functionOutput ++= ".fields["
                                context.functionOutput ++= i.toString()
                                context.functionOutput ++= "])->payload"
                                i+=1
                                context.functionOutput ++= ")"
                                if(i < fields.size){
                                    context.functionOutput ++= " && "
                                }
                            )
                            context.functionOutput ++= " ? "
                            c.head.body.visit(this)
                            context.functionOutput ++= " : "
                            helper(c.tail)
                            context.functionOutput ++= " )"
                        case Wildcard(site) =>


                }
            }
        }

        helper(n.cases)
    def helperPrinter(n : alpine.symbols.Type) (using a: alpine.codegen.CPrinter.Context): Unit = 
        //Either in main or in a function
        if (a.inMain) {
            n match
                case symbols.Type.BuiltinModule => throw Error(s"type '${n}' is not representable in C")
                case symbols.Type.Bool => a.output ++= " %d\", (int)((Int*)"
                case symbols.Type.Int => a.output ++= " %d\", (int)((Int*)"
                case symbols.Type.Float => ???
                case symbols.Type.String => a.output ++= " %s\", (char*)((String*)"
                case symbols.Type.Any => ??? //TODO  
        } else {
            n match
                case symbols.Type.BuiltinModule => throw Error(s"type '${n}' is not representable in C")
                case symbols.Type.Bool => a.output ++= " %d\", (int)((Int*)"
                case symbols.Type.Int => a.functionOutput ++= " %d\", (int)((Int*)"
                case symbols.Type.Float => ???
                case symbols.Type.String => a.functionOutput ++= " %s\", (char*)((String*)"
                case symbols.Type.Any => ??? //TODO
        }

    def visitApplication (n: alpine.ast.Application)(using a: alpine.codegen.CPrinter.Context): Unit =
        //Either in main or in a function 
        if (a.inMain) {
            n.function match
                case Identifier(value, _) =>
                    if value == "print" then

                        if n.arguments.head.value.tpe != symbols.Type.Any then
                            a.output ++= "printf("
                        
                        n.arguments.head.value.tpe match
                            case symbols.Type.BuiltinModule => throw Error(s"type '${n.arguments.head.tpe}' is not representable in C")
                            case symbols.Type.Bool => 
                                a.output ++= "\"%d\", (int)("
                                n.arguments.head.value.visit(this) 
                                a.output ++= ").payload);"
                            case symbols.Type.Int => 
                                a.output ++= "\"%d\", (int)(" 
                                n.arguments.head.value.visit(this) 
                                a.output ++= ").payload);"
                            case symbols.Type.Float => 
                                a.output ++= "\"%f\", *((float *)(" 
                                n.arguments.head.value.visit(this) 
                                a.output ++= ").payload));"
                            case symbols.Type.String => 
                                a.output ++= "\"%s\", (char*)(" 
                                n.arguments.head.value.visit(this) 
                                a.output ++= ").payload);"
                            case symbols.Type.Any => 
                                a.output ++= "if (" 
                                n.arguments.head.value.visit(this) 
                                a.output ++= ".type == IntType) {\n"
                                a.output ++= "printf(\"%d\", (int)(" 
                                n.arguments.head.value.visit(this) 
                                a.output ++= ").payload);\n"
                                a.output ++= "} else if (" 
                                n.arguments.head.value.visit(this) 
                                a.output ++= ".type == BooleanType) {\n"
                                a.output ++= "printf(\"%d\", (int)(" 
                                n.arguments.head.value.visit(this) 
                                a.output ++= ").payload);\n"
                                a.output ++= "} else if (" 
                                n.arguments.head.value.visit(this) 
                                a.output ++= ".type == FloatType) {\n"
                                a.output ++= "printf(\"%f\", *((float *)(" 
                                n.arguments.head.value.visit(this) 
                                a.output ++=").payload));\n"
                                a.output ++= "} else if (" 
                                n.arguments.head.value.visit(this) 
                                a.output ++= ".type == StringType) {\n"
                                a.output ++= "printf(\"%s\", (char*)(" 
                                n.arguments.head.value.visit(this) 
                                a.output ++= ").payload);\n"
                                a.output ++= "}\n"
                                
                            case _: Type => 
                                //GO THROUGHT ALL THE FIELDS OF THE RECORD AND PRINT THEM
                                val name : String = transpiledReferenceTo(n.arguments.head.value.referredEntity.get.entity)
                                val nb_fields : Int = n.arguments.head.value.tpe.asInstanceOf[symbols.Type.Record].fields.size
                                a.output ++= "\"Record : %s\", "
                                a.output ++= name
                                a.output ++= ".name"
                                a.output ++= ");\n"
                               
                                for i <- 0 until nb_fields do
                                    val theField = n.arguments.head.value.tpe.asInstanceOf[symbols.Type.Record].fields(i)
                                    a.output ++= "printf( \", Field nb "
                                    a.output ++= i.toString + " : "
                                    helperPrinter(theField.value)(using a)
                                    a.output ++= name
                                    a.output ++= ".fields["
                                    a.output ++= i.toString
                                    a.output ++= "])->payload);\n"
                                a.output ++= "printf(\", End of the record\");\n"
                                    
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
        } else if (a.isInFunction) {
            n.function match
                case Identifier(value, _) =>
                    if value == "print" then

                        a.functionOutput ++= "printf("

                        n.arguments.head.value.tpe match
                            case symbols.Type.BuiltinModule => throw Error(s"type '${n.arguments.head.tpe}' is not representable in C")
                            case symbols.Type.Bool => 
                                a.functionOutput ++= "\"%d\", ((int)&("
                                n.arguments.head.value.visit(this)
                                a.functionOutput ++= ".payload)))"
                            case symbols.Type.Int => 
                                a.functionOutput ++= "\"%d\", ((int)&("
                                n.arguments.head.value.visit(this)
                                a.functionOutput ++= ".payload)))"
                            case symbols.Type.Float => ??? //TODO
                            case symbols.Type.String => 
                                a.functionOutput ++= "\"%s\", (char*)((String*)"
                                n.arguments.head.value.visit(this)
                                a.functionOutput ++= ")->payload)"
                            case symbols.Type.Any => 
                                a.functionOutput ++= "if ("
                                n.arguments.head.value.visit(this)
                                a.functionOutput ++= ".type == IntType) {\n"
                                a.functionOutput ++= "printf(\"%d\", (int)("
                                n.arguments.head.value.visit(this)
                                a.functionOutput ++= ").payload); \n"
                                a.functionOutput ++= "} else if ("
                                n.arguments.head.value.visit(this)
                                a.functionOutput ++= ".type == BooleanType) {\n"
                                a.functionOutput ++= "printf(\"%d\", (int)("
                                n.arguments.head.value.visit(this) 
                                a.functionOutput ++= ").payload); \n"
                                a.functionOutput ++= "} else if ("
                                n.arguments.head.value.visit(this)
                                a.functionOutput ++= ".type == FloatType) {\n"
                                a.functionOutput ++= "printf(\"%f\", (float)("
                                n.arguments.head.value.visit(this)
                                a.functionOutput ++= ").payload);\n"
                                a.functionOutput ++= "} else if ("
                                n.arguments.head.value.visit(this)
                                a.functionOutput ++= ".type == StringType) {\n"
                                a.functionOutput ++= "printf(\"%s\", (char*)("
                                n.arguments.head.value.visit(this)
                                a.functionOutput ++= ").payload);\n"
                                a.functionOutput ++= "}\n"
                            case _: Type => 
                                //GO THROUGHT ALL THE FIELDS OF THE RECORD AND PRINT THEM
                                val name : String = transpiledReferenceTo(n.arguments.head.value.referredEntity.get.entity)
                                val nb_fields : Int = n.arguments.head.value.tpe.asInstanceOf[symbols.Type.Record].fields.size
                                a.functionOutput ++= "\"Record : %s\", "
                                a.functionOutput ++= name
                                a.functionOutput ++= ".name"
                                a.functionOutput ++= ");\n"
                                for i <- 0 until nb_fields do
                                    val theField = n.arguments.head.value.tpe.asInstanceOf[symbols.Type.Record].fields(i)
                                    a.functionOutput ++= "printf( \", Field nb "
                                    a.functionOutput ++= i.toString 
                                    helperPrinter(theField.value)(using a)
                                    a.functionOutput ++= name
                                    a.functionOutput ++= ".fields["
                                    a.functionOutput ++= i.toString
                                    a.functionOutput ++= "])->payload);\n"
                                a.functionOutput ++= "printf(\", End of the record\")"
                                    
                    else
                        n.function.visit(this)
                        a.functionOutput ++= "("
                        a.functionOutput.appendCommaSeparated(n.arguments) { (o, a) => a.value.visit(this) }
                        a.functionOutput ++= ")"
                case _ => 
                    n.function.visit(this)
                    a.functionOutput ++= "("
                    a.functionOutput.appendCommaSeparated(n.arguments) { (o, a) => a.value.visit(this) }
                    a.functionOutput ++= ")"
        } else {
            n.function match
                case Identifier(value, _) =>
                    if value == "print" then

                        a.initializeGlobals ++= "printf("

                        n.arguments.head.value.tpe match
                            case symbols.Type.BuiltinModule => throw Error(s"type '${n.arguments.head.tpe}' is not representable in C")
                            case symbols.Type.Bool => 
                                a.functionOutput ++= "\"%d\", ((int)&("
                                n.arguments.head.value.visit(this)
                                a.functionOutput ++= ".payload"
                                a.functionOutput ++= ")))"
                            case symbols.Type.Int => 
                                a.initializeGlobals ++= "\"%d\", ((int)&("
                                n.arguments.head.value.visit(this)
                                a.initializeGlobals ++= ".payload"
                                a.initializeGlobals ++= ")))"
                            case symbols.Type.Float => ??? //TODO
                            case symbols.Type.String => 
                                a.initializeGlobals ++= "\"%s\", (char*)((String*)"
                                n.arguments.head.value.visit(this)
                                a.initializeGlobals ++= ")->payload)"
                            case symbols.Type.Any => ??? //TODO
                            case _: Type => 
                                //GO THROUGHT ALL THE FIELDS OF THE RECORD AND PRINT THEM
                                val name : String = transpiledReferenceTo(n.arguments.head.value.referredEntity.get.entity)
                                val nb_fields : Int = n.arguments.head.value.tpe.asInstanceOf[symbols.Type.Record].fields.size
                                a.initializeGlobals ++= "\"Record : %s\", "
                                a.initializeGlobals ++= name
                                a.initializeGlobals ++= ".name"
                                a.initializeGlobals ++= ");\n"
                                for i <- 0 until nb_fields do
                                    val theField = n.arguments.head.value.tpe.asInstanceOf[symbols.Type.Record].fields(i)
                                    a.initializeGlobals ++= "printf( \", Field nb "
                                    a.initializeGlobals ++= i.toString 
                                    helperPrinter(theField.value)(using a)
                                    a.initializeGlobals ++= name
                                    a.initializeGlobals ++= ".fields["
                                    a.initializeGlobals ++= i.toString
                                    a.initializeGlobals ++= "])->payload);\n"
                                a.initializeGlobals ++= "printf(\", End of the record\")"
                                    
                    else
                        n.function.visit(this)
                        a.initializeGlobals ++= "("
                        a.initializeGlobals.appendCommaSeparated(n.arguments) { (o, a) => a.value.visit(this) }
                        a.initializeGlobals ++= ")"
                case _ => 
                    n.function.visit(this)
                    a.initializeGlobals ++= "("
                    a.initializeGlobals.appendCommaSeparated(n.arguments) { (o, a) => a.value.visit(this) }
                    a.initializeGlobals ++= ")"            
        }
        
    def visitArrow (n: alpine.ast.Arrow)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def myVisitAscribed(n: alpine.ast.AscribedExpression, name: String)(using a: alpine.codegen.CPrinter.Context): Unit =
        val inner_name = "ascribed_inner"
        n.operation match
            case alpine.ast.Typecast.Widen => 
                //Create a new global variable with the inner variable in it. 
                a.globalVariables ++= "Any " + name + ";\n"
                n.inner match
                    case ParenthesizedExpression(inner1, site) =>
                        inner1.visit(this)

                        var inner1_name = "ascribed_inner"

                        if (a.ascribed_prev_type == "Float") {
                            inner1_name = inner1_name + "_f"
                        } else if (a.ascribed_prev_type == "Any") {
                            inner1_name = inner1_name 
                        } else if (a.ascribed_prev_type == "Bool") {
                            inner1_name = inner1_name + "_b"
                        } else if (a.ascribed_prev_type == "Int") {
                            inner1_name = inner1_name + "_i"
                        } else if (a.ascribed_prev_type == "String") {
                            inner1_name = inner1_name + "_s"
                        }

                        a.initializeGlobals ++= transpiledType(inner1.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = " + inner1_name + ";\n"

                        a.initializeGlobals ++= inner_name + " = *typeToAny(tmp_" +a.ascribedInnerNumber.toString() + ".payload, tmp_" +a.ascribedInnerNumber.toString() + ".type);\n" 
                        a.initializeGlobals ++= name + " = " + inner_name + ";\n"
                        a.ascribedInnerNumber += 1

                        a.ascribed_prev_type = "Any"

                    case _ => 
                        a.initializeGlobals ++= transpiledType(n.inner.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = " 
                        n.inner.visit(this)
                        a.initializeGlobals ++= ";\n"

                        a.initializeGlobals ++= inner_name + " = *typeToAny(tmp.payload, tmp.type);\n" 
                        a.initializeGlobals ++= name + " = " + inner_name + ";\n"

                        a.ascribed_prev_type = "Any"
                        
            case alpine.ast.Typecast.Narrow =>
                a.globalVariables ++= "Record " + name + ";\n"

                var narrow_ascribed_type = ""
                var narrow_ascribed_intial_of_type = ""
                var narrow_ascribed_Type = ""
                var anyTo_type_function = ""
                n.ascription.tpe match 
                    case symbols.Type.Bool =>
                        narrow_ascribed_type = "BooleanType"
                        narrow_ascribed_intial_of_type = "_b"
                        narrow_ascribed_Type = "Boolean"
                        anyTo_type_function = "anyToBool"
                    case symbols.Type.Int =>
                        narrow_ascribed_type = "IntType"
                        narrow_ascribed_intial_of_type = "_i"
                        narrow_ascribed_Type = "Int"
                        anyTo_type_function = "anyToInt"
                    case symbols.Type.Float =>
                        narrow_ascribed_type = "FloatType"
                        narrow_ascribed_intial_of_type = "_f"
                        narrow_ascribed_Type = "Float"
                        anyTo_type_function = "anyToFloat" 
                    case symbols.Type.String =>
                        narrow_ascribed_type = "StringType"
                        narrow_ascribed_intial_of_type = "_s"
                        narrow_ascribed_Type = "String"
                        anyTo_type_function = "anyToString" 
                    case _ =>

                n.inner match        
                    case ParenthesizedExpression(inner1, site) =>
                        var inner1_name = "ascribed_inner"
                        if (a.ascribed_prev_type == "Float") {
                            inner1_name = inner1_name + "_f"
                        } else if (a.ascribed_prev_type == "Any") {
                            inner1_name = inner1_name 
                        } else if (a.ascribed_prev_type == "Bool") {
                            inner1_name = inner1_name + "_b"
                        } else if (a.ascribed_prev_type == "Int") {
                            inner1_name = inner1_name + "_i"
                        } else if (a.ascribed_prev_type == "String") {
                            inner1_name = inner1_name + "_s"
                        }

                        inner1.visit(this)

                        if (transpiledType(inner1.tpe) != "Any")
                            a.initializeGlobals ++= "void* tmp_field"+ a.ascribedInnerNumber.toString() +"[1];\n"
                            a.initializeGlobals ++= "tmp_field" + a.ascribedInnerNumber.toString() + "[0] = &((String) {.payload = (void*)\"none\", .type = StringType});\n"
                            a.initializeGlobals ++= name + " = *createRecord(1, tmp_field" + a.ascribedInnerNumber.toString() +  ", \"none\");\n"
                        else 

                            a.initializeGlobals ++= transpiledType(inner1.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = " + inner1_name + ";\n"
                        
                            a.initializeGlobals ++= "if (tmp_" + a.ascribedInnerNumber.toString() + ".type == " + narrow_ascribed_type + " ) {\n" //BooleanType

                            a.initializeGlobals ++= inner_name + narrow_ascribed_intial_of_type + " = *" + anyTo_type_function + "(tmp_" + a.ascribedInnerNumber.toString() + ".payload);\n" //_b - anyToBool
                            a.initializeGlobals ++= "void* tmp_field"+ a.ascribedInnerNumber.toString() +"[1];\n"
                            a.initializeGlobals ++= "tmp_field" + a.ascribedInnerNumber.toString() + "[0] = &" + inner_name + narrow_ascribed_intial_of_type + ";\n"
                            a.initializeGlobals ++= name + " = *createRecord(1, tmp_field" + a.ascribedInnerNumber.toString() +  ", \"some\");\n"                                    
                            a.ascribedInnerNumber += 1

                            a.ascribed_prev_type = "Some(" + narrow_ascribed_Type + ")" //Boolean
                            a.initializeGlobals ++= "} else {\n"
                            a.initializeGlobals ++= "void* tmp_field"+ a.ascribedInnerNumber.toString() +"[1];\n"
                            a.initializeGlobals ++= "tmp_field" + a.ascribedInnerNumber.toString() + "[0] = &((String) {.payload = (void*)\"none\", .type = StringType});\n"
                            a.initializeGlobals ++= name + " = *createRecord(1, tmp_field" + a.ascribedInnerNumber.toString() +  ", \"none\");\n"
                            a.initializeGlobals ++= "}\n"
                    case _ =>
                        if (transpiledType(n.inner.tpe) != "Any")
                            a.initializeGlobals ++= "void* tmp_field"+ a.ascribedInnerNumber.toString() +"[1];\n"
                            a.initializeGlobals ++= "tmp_field" + a.ascribedInnerNumber.toString() + "[0] = &((String) {.payload = (void*)\"none\", .type = StringType});\n"
                            a.initializeGlobals ++= name + " = *createRecord(1, tmp_field" + a.ascribedInnerNumber.toString() +  ", \"none\");\n"
                        else 
                            a.initializeGlobals ++= transpiledType(n.inner.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = " 
                            n.inner.visit(this)
                            a.initializeGlobals ++= ";\n"

                            a.initializeGlobals ++= inner_name + narrow_ascribed_intial_of_type + " = *" + anyTo_type_function + "(tmp_" + a.ascribedInnerNumber.toString() + ".payload);\n" 
                            a.initializeGlobals ++= "void* tmp_field"+ a.ascribedInnerNumber.toString() +"[1];\n"
                            a.initializeGlobals ++= "tmp_field" + a.ascribedInnerNumber.toString() + "[0] = &" + inner_name + narrow_ascribed_intial_of_type + ";\n"
                            a.initializeGlobals ++= name + " = *createRecord(1, tmp_field" + a.ascribedInnerNumber.toString() +  ", \"some\");\n"                                    
                            a.ascribedInnerNumber += 1

                            a.ascribed_prev_type = "Some(" + narrow_ascribed_Type + ")" //Boolean
            case alpine.ast.Typecast.NarrowUnconditionally =>
                n.ascription.tpe match 
                    case symbols.Type.Bool => 
                        a.globalVariables ++= "Boolean " + name + ";\n"
                        n.inner match
                            case ParenthesizedExpression(inner1, site) =>

                                var inner1_name = "ascribed_inner"

                                if (a.ascribed_prev_type == "Float") {
                                    inner1_name = inner1_name + "_f"
                                } else if (a.ascribed_prev_type == "Any") {
                                    inner1_name = inner1_name 
                                } else if (a.ascribed_prev_type == "Bool") {
                                    inner1_name = inner1_name + "_b"
                                } else if (a.ascribed_prev_type == "Int") {
                                    inner1_name = inner1_name + "_i"
                                } else if (a.ascribed_prev_type == "String") {
                                    inner1_name = inner1_name + "_s"
                                }

                                inner1.visit(this)

                                a.initializeGlobals ++= transpiledType(inner1.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = " + inner1_name + ";\n"

                                a.initializeGlobals ++= inner_name + "_b" + " = *anyToBool(tmp_" + a.ascribedInnerNumber.toString() + ".payload);\n" 
                                a.initializeGlobals ++= name + " = " + inner_name + "_b" + ";\n"
                                a.ascribedInnerNumber += 1

                                a.ascribed_prev_type = "Boolean"
                            case _ =>
                                a.initializeGlobals ++= transpiledType(n.inner.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = "
                                n.inner.visit(this)
                                a.initializeGlobals ++= ";\n"

                                a.initializeGlobals ++= inner_name + "_b" + " = *anyToBool(tmp_" +a.ascribedInnerNumber.toString() + ".payload);\n" 
                                a.initializeGlobals ++= name + " = " + inner_name + "_b" + ";\n"

                                a.ascribed_prev_type = "Boolean"

                    case symbols.Type.Int => 
                        a.globalVariables ++= "Int " + name + ";\n"
                        n.inner match
                            case ParenthesizedExpression(inner1, site) =>

                                var inner1_name = "ascribed_inner"

                                if (a.ascribed_prev_type == "Float") {
                                    inner1_name = inner1_name + "_f"
                                } else if (a.ascribed_prev_type == "Any") {
                                    inner1_name = inner1_name 
                                } else if (a.ascribed_prev_type == "Bool") {
                                    inner1_name = inner1_name + "_b"
                                } else if (a.ascribed_prev_type == "Int") {
                                    inner1_name = inner1_name + "_i"
                                } else if (a.ascribed_prev_type == "String") {
                                    inner1_name = inner1_name + "_s"
                                }

                                inner1.visit(this)

                                a.initializeGlobals ++= transpiledType(inner1.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = " + inner1_name + ";\n"

                                a.initializeGlobals ++= inner_name + "_i" + " = *anyToInt(tmp_" + a.ascribedInnerNumber.toString() + ".payload);\n" 
                                a.initializeGlobals ++= name + " = " + inner_name + "_i" + ";\n"
                                a.ascribedInnerNumber += 1

                                a.ascribed_prev_type = "Int"
                            case _ =>
                                
                                a.initializeGlobals ++= transpiledType(n.inner.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = "
                                n.inner.visit(this)
                                a.initializeGlobals ++= ";\n"

                                a.initializeGlobals ++= inner_name + "_i" + " = *anyToInt(tmp_" +a.ascribedInnerNumber.toString() + ".payload);\n" 
                                a.initializeGlobals ++= name + " = " + inner_name + "_i" + ";\n"

                                a.ascribed_prev_type = "Int"

                    case symbols.Type.Float => 
                        a.globalVariables ++= "Float " + name + ";\n"
                        n.inner match
                            case ParenthesizedExpression(inner1, site) =>

                                var inner1_name = "ascribed_inner"

                                if (a.ascribed_prev_type == "Float") {
                                    inner1_name = inner1_name + "_f"
                                } else if (a.ascribed_prev_type == "Any") {
                                    inner1_name = inner1_name 
                                } else if (a.ascribed_prev_type == "Bool") {
                                    inner1_name = inner1_name + "_b"
                                } else if (a.ascribed_prev_type == "Int") {
                                    inner1_name = inner1_name + "_i"
                                } else if (a.ascribed_prev_type == "String") {
                                    inner1_name = inner1_name + "_s"
                                }

                                inner1.visit(this)

                                a.initializeGlobals ++= transpiledType(inner1.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = " + inner1_name + ";\n"

                                a.initializeGlobals ++= inner_name + "_b" + " = *anyToFloat(tmp_" + a.ascribedInnerNumber.toString() + ".payload);\n" 
                                a.initializeGlobals ++= name + " = " + inner_name + "_f" + ";\n"
                                a.ascribedInnerNumber += 1

                                a.ascribed_prev_type = "Float"
                            case _ =>
                                a.initializeGlobals ++= transpiledType(n.inner.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = "
                                n.inner.visit(this)
                                a.initializeGlobals ++= ";\n"

                                a.initializeGlobals ++= inner_name + "_b" + " = *anyToFloat(tmp_" +a.ascribedInnerNumber.toString() + ".payload);\n" 
                                a.initializeGlobals ++= name + " = " + inner_name + "_f" + ";\n"

                                a.ascribed_prev_type = "Float"
                                
                    case symbols.Type.String => 
                        a.globalVariables ++= "String " + name + ";\n"
                        n.inner match
                            case ParenthesizedExpression(inner1, site) =>

                                var inner1_name = "ascribed_inner"

                                if (a.ascribed_prev_type == "Float") {
                                    inner1_name = inner1_name + "_f"
                                } else if (a.ascribed_prev_type == "Any") {
                                    inner1_name = inner1_name 
                                } else if (a.ascribed_prev_type == "Bool") {
                                    inner1_name = inner1_name + "_b"
                                } else if (a.ascribed_prev_type == "Int") {
                                    inner1_name = inner1_name + "_i"
                                } else if (a.ascribed_prev_type == "String") {
                                    inner1_name = inner1_name + "_s"
                                }

                                inner1.visit(this)

                                a.initializeGlobals ++= transpiledType(inner1.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = " + inner1_name + ";\n"

                                a.initializeGlobals ++= inner_name + "_s" + " = *anyToString(tmp_" + a.ascribedInnerNumber.toString() + ".payload);\n" 
                                a.initializeGlobals ++= name + " = " + inner_name + "_s" + ";\n"
                                a.ascribedInnerNumber += 1

                                a.ascribed_prev_type = "String"
                            case _ =>
                                a.initializeGlobals ++= transpiledType(n.inner.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = "
                                n.inner.visit(this)
                                a.initializeGlobals ++= ";\n"

                                a.initializeGlobals ++= inner_name + "_s" + " = *anyToString(tmp_" +a.ascribedInnerNumber.toString() + ".payload);\n" 
                                a.initializeGlobals ++= name + " = " + inner_name + "_s" + ";\n"

                                a.ascribed_prev_type = "String"
                    case symbols.Type.Any => 
                        a.globalVariables ++= "Any " + name + ";\n"
                    case _ =>
                        

        

        
    def visitAscribedExpression(n: alpine.ast.AscribedExpression)(using a: alpine.codegen.CPrinter.Context): Unit = 
        val inner_name = "ascribed_inner"
        n.operation match
            case alpine.ast.Typecast.Widen =>
                //Create a new global variable with the inner variable in it. 

                n.inner match
                    case ParenthesizedExpression(inner1, site) =>
                        inner1.visit(this)

                        val inner1_name = "ascribed_inner"
                        a.initializeGlobals ++= transpiledType(inner1.tpe) + " tmp_" + a.ascribedInnerNumber.toString()  + " = " + inner1_name + ";\n"
                        a.initializeGlobals ++= inner_name + " = *typeToAny(tmp_" +a.ascribedInnerNumber.toString() + ".payload, tmp_" +a.ascribedInnerNumber.toString() + ".type);\n"
                        a.ascribedInnerNumber += 1

                    case _ =>
                        a.initializeGlobals ++= transpiledType(n.inner.tpe) + " tmp_" + a.ascribedInnerNumber.toString()  + " = "
                        n.inner.visit(this)
                        a.initializeGlobals ++= ";\n"

                        a.initializeGlobals ++= inner_name + " = *typeToAny(tmp_" +a.ascribedInnerNumber.toString() + ".payload, tmp_" +a.ascribedInnerNumber.toString() + ".type);\n"
                        a.ascribedInnerNumber += 1
                
            case alpine.ast.Typecast.Narrow =>

            case alpine.ast.Typecast.NarrowUnconditionally =>
                n.ascription.tpe match 
                    case symbols.Type.Bool => 
                        n.inner match
                            case ParenthesizedExpression(inner1, site) =>

                                var inner1_name = "ascribed_inner"

                                if (a.ascribed_prev_type == "Float") {
                                    inner1_name = inner1_name + "_f"
                                } else if (a.ascribed_prev_type == "Any") {
                                    inner1_name = inner1_name 
                                } else if (a.ascribed_prev_type == "Bool") {
                                    inner1_name = inner1_name + "_b"
                                } else if (a.ascribed_prev_type == "Int") {
                                    inner1_name = inner1_name + "_i"
                                } else if (a.ascribed_prev_type == "String") {
                                    inner1_name = inner1_name + "_s"
                                }

                                inner1.visit(this)

                                a.initializeGlobals ++= transpiledType(inner1.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = " + inner1_name + ";\n"

                                a.initializeGlobals ++= inner_name + "_b" + " = *anyToBool(tmp_" + a.ascribedInnerNumber.toString() + ".payload);\n" 
                                a.ascribedInnerNumber += 1

                                a.ascribed_prev_type = "Boolean"
                            case _ =>
                                a.initializeGlobals ++= transpiledType(n.inner.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = "
                                n.inner.visit(this)
                                a.initializeGlobals ++= ";\n"

                                a.initializeGlobals ++= inner_name + "_b" + " = *anyToBool(tmp_" +a.ascribedInnerNumber.toString() + ".payload);\n" 

                                a.ascribed_prev_type = "Boolean"

                    case symbols.Type.Int => 
                        n.inner match
                            case ParenthesizedExpression(inner1, site) =>

                                var inner1_name = "ascribed_inner"

                                if (a.ascribed_prev_type == "Float") {
                                    inner1_name = inner1_name + "_f"
                                } else if (a.ascribed_prev_type == "Any") {
                                    inner1_name = inner1_name 
                                } else if (a.ascribed_prev_type == "Bool") {
                                    inner1_name = inner1_name + "_b"
                                } else if (a.ascribed_prev_type == "Int") {
                                    inner1_name = inner1_name + "_i"
                                } else if (a.ascribed_prev_type == "String") {
                                    inner1_name = inner1_name + "_s"
                                }

                                inner1.visit(this)

                                a.initializeGlobals ++= transpiledType(inner1.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = " + inner1_name + ";\n"

                                a.initializeGlobals ++= inner_name + "_i" + " = *anyToInt(tmp_" + a.ascribedInnerNumber.toString() + ".payload);\n" 
                                a.ascribedInnerNumber += 1

                                a.ascribed_prev_type = "Int"
                            case _ =>
                                
                                a.initializeGlobals ++= transpiledType(n.inner.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = "
                                n.inner.visit(this)
                                a.initializeGlobals ++= ";\n"

                                a.initializeGlobals ++= inner_name + "_i" + " = *anyToInt(tmp_" +a.ascribedInnerNumber.toString() + ".payload);\n" 

                                a.ascribed_prev_type = "Int"

                    case symbols.Type.Float => 
                        n.inner match
                            case ParenthesizedExpression(inner1, site) =>

                                var inner1_name = "ascribed_inner"

                                if (a.ascribed_prev_type == "Float") {
                                    inner1_name = inner1_name + "_f"
                                } else if (a.ascribed_prev_type == "Any") {
                                    inner1_name = inner1_name 
                                } else if (a.ascribed_prev_type == "Bool") {
                                    inner1_name = inner1_name + "_b"
                                } else if (a.ascribed_prev_type == "Int") {
                                    inner1_name = inner1_name + "_i"
                                } else if (a.ascribed_prev_type == "String") {
                                    inner1_name = inner1_name + "_s"
                                }

                                inner1.visit(this)

                                a.initializeGlobals ++= transpiledType(inner1.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = " + inner1_name + ";\n"

                                a.initializeGlobals ++= inner_name + "_b" + " = *anyToFloat(tmp_" + a.ascribedInnerNumber.toString() + ".payload);\n" 
                                a.ascribedInnerNumber += 1

                                a.ascribed_prev_type = "Float"
                            case _ =>
                                a.initializeGlobals ++= transpiledType(n.inner.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = "
                                n.inner.visit(this)
                                a.initializeGlobals ++= ";\n"

                                a.initializeGlobals ++= inner_name + "_b" + " = *anyToFloat(tmp_" +a.ascribedInnerNumber.toString() + ".payload);\n" 

                                a.ascribed_prev_type = "Float"
                                
                    case symbols.Type.String => 
                        n.inner match
                            case ParenthesizedExpression(inner1, site) =>

                                var inner1_name = "ascribed_inner"

                                if (a.ascribed_prev_type == "Float") {
                                    inner1_name = inner1_name + "_f"
                                } else if (a.ascribed_prev_type == "Any") {
                                    inner1_name = inner1_name 
                                } else if (a.ascribed_prev_type == "Bool") {
                                    inner1_name = inner1_name + "_b"
                                } else if (a.ascribed_prev_type == "Int") {
                                    inner1_name = inner1_name + "_i"
                                } else if (a.ascribed_prev_type == "String") {
                                    inner1_name = inner1_name + "_s"
                                }

                                inner1.visit(this)

                                a.initializeGlobals ++= transpiledType(inner1.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = " + inner1_name + ";\n"

                                a.initializeGlobals ++= inner_name + "_s" + " = *anyToString(tmp_" + a.ascribedInnerNumber.toString() + ".payload);\n" 
                                a.ascribedInnerNumber += 1

                                a.ascribed_prev_type = "String"
                            case _ =>
                                a.initializeGlobals ++= transpiledType(n.inner.tpe) + " tmp_" + a.ascribedInnerNumber.toString() + " = "
                                n.inner.visit(this)
                                a.initializeGlobals ++= ";\n"

                                a.initializeGlobals ++= inner_name + "_s" + " = *anyToString(tmp_" +a.ascribedInnerNumber.toString() + ".payload);\n" 

                                a.ascribed_prev_type = "String"
                    case symbols.Type.Any => 
                    case _ =>
        
            
            



                
        

    def visitBooleanLiteral(n: alpine.ast.BooleanLiteral)(using a: alpine.codegen.CPrinter.Context): Unit =
        if (a.inMain) {
            a.output ++= makeBoolLiteral(n.value)
        } else if (a.isInFunction) {
            a.functionOutput ++= makeBoolLiteral(n.value)
        } else {
            a.initializeGlobals ++= makeBoolLiteral(n.value)
        }

    def visitError(n: alpine.ast.ErrorTree)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitFloatLiteral(n: alpine.ast.FloatLiteral)(using a: alpine.codegen.CPrinter.Context): Unit = 
        val name = "tmp_float" + a.float_counter.toString()
        a.float_counter += 1
        if (a.inMain) {
            a.output.insert(0, "float* " + name + "= malloc(sizeof(float));\n" + "*" + name + " = " + n.value.toString + ";\n")
            a.output ++= makeFloatLiteral(name)
        } else if (a.isInFunction) {
            a.functionOutput.insert(0, "float* " + name + "= malloc(sizeof(float));\n" + "*" + name + " = " + n.value.toString + ";\n")
            a.float_counter += 1
            a.functionOutput ++= makeFloatLiteral(name)
        } else {
            a.initializeGlobals.insert(0, "float* " + name + "= malloc(sizeof(float));\n" + "*" + name + " = " + n.value.toString + ";\n")
            a.float_counter += 1
            a.initializeGlobals ++= makeFloatLiteral(name)
        }

    def visitIdentifier(n: alpine.ast.Identifier)(using a: alpine.codegen.CPrinter.Context): Unit = 
        if (a.inMain) {
            a.output ++= transpiledReferenceTo(n.referredEntity.get.entity)
        } else {
            a.initializeGlobals ++= transpiledReferenceTo(n.referredEntity.get.entity)
        }

    def visitInfixApplication(n: alpine.ast.InfixApplication)(using a: alpine.codegen.CPrinter.Context): Unit = 
        if (a.inMain) {
            n.function.visit(this)
            a.output ++= "("
            n.lhs.visit(this)
            a.output ++= ", "
            n.rhs.visit(this)
            a.output ++= ")"
        } else if (a.isInFunction) {
            n.function.visit(this)
            a.functionOutput ++= "("
            n.lhs.visit(this)
            a.functionOutput ++= ", "
            n.rhs.visit(this)
            a.functionOutput ++= ")"
        } else {
            n.function.visit(this)
            a.initializeGlobals ++= "("
            n.lhs.visit(this)
            a.initializeGlobals ++= ", "
            n.rhs.visit(this)
            a.initializeGlobals ++= ")"
        }

    def visitIntegerLiteral(n: alpine.ast.IntegerLiteral)(using a: alpine.codegen.CPrinter.Context): Unit = 
        if (a.inMain) {
            a.output ++= makeInt(n.value)
        } else if (a.isInFunction) {
            a.functionOutput ++= makeInt(n.value)
        } else {
            a.initializeGlobals ++= makeInt(n.value)
        }

    def visitLabeled[T <: alpine.ast.Tree](n: alpine.ast.Labeled[T])(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitLambda(n: alpine.ast.Lambda)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitMatchCase (n: alpine.ast.Match.Case)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitParameter (n: alpine.ast.Parameter)(using a: alpine.codegen.CPrinter.Context): Unit = 
        unexpectedVisit(n)

    def visitParenthesizedType(n: alpine.ast.ParenthesizedType)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitPrefixApplication(n: alpine.ast.PrefixApplication)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitRecord(n: alpine.ast.Record)(using a: alpine.codegen.CPrinter.Context): Unit = 
        a.output ++= ""
    
    def creatUniqueRecordName(n: alpine.ast.Record) : String =
        var name = StringBuilder()
        name ++= n.identifier + "_"
        var types = StringBuilder()
        for f <- n.fields do
            types ++= discriminator(f.value.tpe)
        name ++= types.toString.sorted
        name.toString().filter(_ != ' ').filter(_ != '#')

    def myVisitRecord(n: alpine.ast.Record, name : String)(using a: alpine.codegen.CPrinter.Context): Unit =
        if (a.inMain) { 
            val field_name : String = name + "_fields"
            a.output ++= "void* "
            a.output ++= field_name
            a.output ++= "["
            a.output ++= n.fields.size.toString
            a.output ++= "];\n"

            var i = 0
            for f <- n.fields do
                //if (f.value.tpe == alpine.ast.Record) 
                //   myVisitRecord(f, name + "_0")
                val i_type = transpiledType(f.value.tpe)
                val tmp_name = "tmp_" + i.toString + "_" + field_name
                a.output ++= i_type + "* " + tmp_name + " = malloc(sizeof(" + i_type + "));\n"
                a.output ++= "*" + tmp_name + " = "
                f.value.visit(this)
                a.output ++= ";\n"
                a.output ++= field_name
                a.output ++= "["
                a.output ++= i.toString
                i += 1
                a.output ++= "] = " + tmp_name + ";\n"

            a.output ++= "Record "
            a.output ++= name
            a.output ++= " = *createRecord("
            a.output ++= n.fields.size.toString
            a.output ++= ", "
            a.output ++= field_name
            a.output ++= ", "
            val unique_name = creatUniqueRecordName(n)
            a.output ++= "\"" + unique_name + "\""
            a.output ++= ");\n"
        } else if (a.isInFunction) {
            val field_name : String = name + "_fields"
            a.functionOutput ++= "void* "
            a.functionOutput ++= field_name
            a.functionOutput ++= "["
            a.functionOutput ++= n.fields.size.toString
            a.functionOutput ++= "];\n"

            var i = 0
            for f <- n.fields do
                val i_type = transpiledType(f.value.tpe)
                val tmp_name = "tmp_" + i.toString + "_" + field_name
                a.functionOutput ++= i_type + "* " + tmp_name + " = malloc(sizeof(" + i_type + "));\n"
                a.functionOutput ++= "*" + tmp_name + " = "
                f.value.visit(this)
                a.functionOutput ++= ";\n"
                a.functionOutput ++= field_name
                a.functionOutput ++= "["
                a.functionOutput ++= i.toString
                i += 1
                a.functionOutput ++= "] = " + tmp_name + ";\n"

            a.functionOutput ++= "Record "
            a.functionOutput ++= name
            a.functionOutput ++= " = *createRecord("
            a.functionOutput ++= n.fields.size.toString
            a.functionOutput ++= ", "
            a.functionOutput ++= field_name
            a.functionOutput ++= ", "
            val unique_name = creatUniqueRecordName(n)
            a.functionOutput ++= "\"" + unique_name + "\""
            a.functionOutput ++= ");\n"
        } else {
            val field_name : String = name + "_fields"
            a.initializeGlobals ++= "void* "
            a.initializeGlobals ++= field_name
            a.initializeGlobals ++= "["
            a.initializeGlobals ++= n.fields.size.toString
            a.initializeGlobals ++= "];\n"

            var i = 0
            for f <- n.fields do
                val i_type = transpiledType(f.value.tpe)
                val tmp_name = "tmp_" + i.toString + "_" + field_name
                a.initializeGlobals ++= i_type + "* " + tmp_name + " = malloc(sizeof(" + i_type + "));\n"
                a.initializeGlobals ++= "*" + tmp_name + " = "
                f.value.visit(this)
                a.initializeGlobals ++= ";\n"
                a.initializeGlobals ++= field_name
                a.initializeGlobals ++= "["
                a.initializeGlobals ++= i.toString
                i += 1
                a.initializeGlobals ++= "] = " + tmp_name + ";\n"

            a.globalVariables ++= "Record " + name + ";\n"
            a.initializeGlobals ++= name
            a.initializeGlobals ++= " = *createRecord("
            a.initializeGlobals ++= n.fields.size.toString
            a.initializeGlobals ++= ", "
            a.initializeGlobals ++= field_name
            a.initializeGlobals ++= ", "
            val unique_name = creatUniqueRecordName(n)
            a.initializeGlobals ++= "\"" + unique_name + "\""
            a.initializeGlobals ++= ");\n"
        }

    def myVisitRecordAsString(n: alpine.ast.Record, name : String)(using a: alpine.codegen.CPrinter.Context): String =
        val field_name : String = name + "_fields"
        var res = StringBuilder()
        res ++= "void* "
        res ++= field_name
        res ++= "["
        res ++= n.fields.size.toString
        res ++= "];\n"

        var i = 0
        for f <- n.fields do
            val i_type = transpiledType(f.value.tpe)
            val tmp_name = "tmp_" + i.toString + "_" + field_name
            res ++= i_type + "* " + tmp_name + " = malloc(sizeof(" + i_type + "));\n"
            res ++= "*" + tmp_name + " = "
            f.value match
                case ErrorTree(site) => 
                case Identifier(value, site) =>
                case alpine.ast.BooleanLiteral(value, site) =>
                    res ++= makeBoolLiteral(value)
                case alpine.ast.FloatLiteral(value, site) =>
                    res ++= makeFloatLiteral(value)
                case alpine.ast.StringLiteral(value, site) =>
                    res ++= makeStringLiteral(value)
                case IntegerLiteral(value, site) =>
                    res ++= makeInt(value)
                case _: Literal => 
                case alpine.ast.Record(identifier, fields, site) =>
                case Selection(qualification, selectee, site) =>
                case Application(function, arguments, site) =>
                case PrefixApplication(function, argument, site) =>
                case InfixApplication(function, lhs, rhs, site) =>
                case Conditional(condition, successCase, failureCase, site) =>
                case Match(scrutinee, cases, site) =>
                case Let(binding, body, site) =>
                case Lambda(inputs, output, body, site) =>
                case ParenthesizedExpression(inner, site) =>
                case AscribedExpression(inner, operation, ascription, site) =>
            
            res ++= ";\n"
            res ++= field_name
            res ++= "["
            res ++= i.toString
            i += 1
            res ++= "] = " + tmp_name + ";\n"

        res ++= "Record "
        res ++= name
        res ++= " = *createRecord("
        res ++= n.fields.size.toString
        res ++= ", "
        res ++= field_name
        res ++= ", "
        val unique_name = creatUniqueRecordName(n)
        res ++= "\"" + unique_name + "\""
        res ++= ");\n"
        res.toString()
    def visitRecordPattern (n: alpine.ast.RecordPattern)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitRecordType(n: alpine.ast.RecordType)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitSelection(n: alpine.ast.Selection)(using a: alpine.codegen.CPrinter.Context): Unit = 
        // WE WANT SOMETHING LIKE : *(String *)_rRrecordSI.fields[0]
        if (a.isInFunction == false) {
            if a.inMain then
                a.output ++= "*("
                a.output ++= transpiledType(n.tpe)
                a.output ++= "*)"
                n.qualification.visit(this)
                n.referredEntity match
                case Some(symbols.EntityReference(e: symbols.Entity.Field, _)) =>
                    a.output ++= ".fields[" + e.index + "]"
                case _ =>
                    unexpectedVisit(n.selectee)
            else
                a.initializeGlobals ++= "*("
                a.initializeGlobals ++= transpiledType(n.tpe)
                a.initializeGlobals ++= "*)"
                n.qualification.visit(this)
                n.referredEntity match
                case Some(symbols.EntityReference(e: symbols.Entity.Field, _)) =>
                    a.initializeGlobals ++= ".fields[" + e.index + "]"
                case _ =>
                    unexpectedVisit(n.selectee)
        } else {
            n.qualification.visit(this)
            n.referredEntity match
            case Some(symbols.EntityReference(e: symbols.Entity.Field, _)) =>
                a.functionOutput ++= ".fields[" + e.index + "]"
            case _ =>
                unexpectedVisit(n.selectee)
        }
        

    def visitStringLiteral (n: alpine.ast.StringLiteral)(using a: alpine.codegen.CPrinter.Context): Unit = 
        if (a.inMain) {
            a.output ++= makeStringLiteral(n.value)
        } else if (a.isInFunction) {
            a.functionOutput ++= makeStringLiteral(n.value)
        } else {
            a.initializeGlobals ++= makeStringLiteral(n.value)
        }

    def visitSum(n: alpine.ast.Sum)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitTypeApplication (n: alpine.ast.TypeApplication) (using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitTypeDeclaration (n: alpine.ast.TypeDeclaration) (using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitTypeIdentifier (n: alpine.ast.TypeIdentifier)(using a: alpine.codegen.CPrinter.Context): Unit = ???

    def visitValuePattern (n: alpine.ast.ValuePattern)(using a: alpine.codegen.CPrinter.Context): Unit = 
        //Will come here when record on match. Hence we want to extract the name of the record
        //Either it is a record or it is a parenthesized expression with a record as inner body
        //If it is a parenthesized expression, we want to extract the record
        n.value.visit(this)

    def visitWildcard (n: alpine.ast.Wildcard)(using a: alpine.codegen.CPrinter.Context): Unit = ???



    /** Returns a string uniquely identifiyng t for use as a discriminator in a mangled name. */
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

    /** Returns a string uniquely identifiyng t for use as a discriminator in a mangled name. */
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

    /** Returns a string uniquely identifiyng t for use as a discriminator in a mangled name. */
    private def discriminator(t: symbols.Type.Arrow): String =
        val b = StringBuilder("X")
        for i <- t.inputs do
            b ++= i.label.getOrElse("")
            b ++= discriminator(i.value)
            b ++= discriminator(t.output)
        b.toString

    /** Returns a string uniquely identifiyng t for use as a discriminator in a mangled name. */
    private def discriminator(t: symbols.Type.Sum): String =
        if t.members.isEmpty then "N" else
        "E" + t.members.map(discriminator).mkString


object CPrinter:

    final class Context():

        /** true iff the transpiler is processing an expression of type Any. */
        var isAny = false

        var inMain = false

        var isInFunction = false

        var ascribedInnerNumber = 0

        var ascribed_prev_type = "Any"

        var float_counter = 0

        /** The types that must be emitted in the program. */
        private var _typesToEmit = mutable.Set[symbols.Type.Record]()

        /** The types that must be emitted in the program. */
        def typesToEmit: Set[symbols.Type.Record] = _typesToEmit.toSet

        /** The (partial) result of the transpilation. */
        private var _output = StringBuilder()
        def output: StringBuilder = _output

        private var _functionOutput = StringBuilder()
        def functionOutput: StringBuilder = _functionOutput

        private var _initialOutput = StringBuilder()
        def initialOutput: StringBuilder = _initialOutput

        private var _globalVariables = StringBuilder()
        def globalVariables: StringBuilder = _globalVariables

        private var _initializeGlobals = StringBuilder()
        def initializeGlobals: StringBuilder = _initializeGlobals

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