package alpine
package parsing

import alpine.ast
import alpine.util.FatalError

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.SeqView.Reverse
import scala.annotation.meta.param

class Parser(val source: SourceFile):

  import ast.*
  import Token.Kind as K

  /** The token stream. */
  private var tokens = Lexer(source)

  /** The boundary of the last consumed token. */
  private var lastBoundary = 0

  /** The next token in the stream if it has already been produced. */
  private var lookahead: Option[Token] = None

  /** The errors collected by the parser. */
  private var errors = mutable.ListBuffer[SyntaxError]()

  /** A stack of predicates identifying tokens suitable for error recovery. */
  private var recoveryPredicates = mutable.ListBuffer[Token => Boolean]()

  /** The diagnostics collected by the parser. */
  def diagnostics: DiagnosticSet =
    DiagnosticSet(errors)

  // --- Declarations ---------------------------------------------------------

  /** Parses and returns a program. */
  def program(): alpine.Program =
    @tailrec def loop(partialResult: List[Declaration]): IArray[Declaration] =
      if peek != None then
        loop(partialResult :+ topLevel())
      else
        IArray.from(partialResult)
    Program(loop(List()))

  /** Parses and returns a top-level declaration. */
  def topLevel(): Declaration =
    peek match
      case Some(Token(K.Let, _)) =>
        binding()
      case Some(Token(K.Fun, _)) =>
        function()
      case Some(Token(K.Type, _)) =>
        typeDeclaration()
      case _ =>
        recover(ExpectedTree("top-level declaration", emptySiteAtLastBoundary), ErrorTree.apply)

  /** Parses and returns a binding declaration. */
  private[parsing] def binding(initializerIsExpected: Boolean = true): Binding =
    val start = lastBoundary
    expect(K.Let)
    val id = identifier()
    peek match
      case Some(Token(K.Eq, _)) => 
        take()
        Binding(id.value, None, Some(expression()),  source.span(start, lastBoundary))
      case Some(Token(K.Colon, _)) =>
        take()
        val tp = tpe()
        if(!initializerIsExpected) then
          peek match
            case Some(Token(K.Eq, _)) =>
              take()
              Binding(id.value, Some(tp), Some(expression()), source.span(start, lastBoundary))
            case _ =>
              Binding(id.value, Some(tp), None, source.span(start, lastBoundary))
        else
          if(peek == None || !K.Eq.matches(take().get)) then report(SyntaxError("intit exprected", source.span(start, lastBoundary)))
          Binding(id.value, Some(tp), Some(expression()),source.span(start, lastBoundary))
      case _ => 
        if(!initializerIsExpected) then 
          Binding(id.value, None, None, source.span(start, lastBoundary))
        else
          report(SyntaxError("intit exprected", source.span(start, lastBoundary)))
          Binding("init expected", None, None, source.span(0, 0))

  /** Parses and returns a function declaration. */
  private[parsing] def function(): Function =
    //A LA MAIN
    val start = lastBoundary
    expect(K.Fun)
    val id = functionIdentifier()
    val lab = valueParameterList()
    peek match
      case(Some(Token(K.Arrow,_))) =>
        take()
        val tp = tpe()
        expect(K.LBrace)
        val exp = expression()
        expect(K.RBrace)
        Function(id, List.empty, lab, Some(tp), exp, source.span(start, lastBoundary))
      case Some(Token(K.LBrace,_)) =>
        expect(K.LBrace)
        val expr = expression()
        expect(K.RBrace)
        Function(id, List.empty, lab, None, expr, source.span(start, lastBoundary))
      case _   => Function(id, List.empty, lab, None, Identifier("error",source.span(start, lastBoundary)), source.span(start, lastBoundary))

  /** Parses and returns the identifier of a function. */
  private def functionIdentifier(): String =
    //ALAMAIN
    take(K.Identifier) match
      case Some(s) =>
        s.site.text.toString
      case _ if peek.map((t) => t.kind.isOperatorPart).getOrElse(false) =>
        operatorIdentifier()(1).text.toString
      case _ =>
        missingName

  /** Parses and returns a list of parameter declarations in parentheses. */
  private[parsing] def valueParameterList(): List[Parameter] =
    //ALAMAIN
    expect(K.LParen)
    val list = commaSeparatedList( scrutinee  =>K.RParen.matches(scrutinee), ()=> parameter() ).collect({case param: Parameter => param})
    take()
    list

/** Parses and returns a parameter declaration. */
  private[parsing] def parameter(): Declaration =
    //ALAMAIN
    val start = lastBoundary
    peek match
      case Some(Token(k, s)) if k.isKeyword => 
        take()
        val end = lastBoundary
        Parameter(Some(k.toString.toLowerCase()), take(K.Identifier).map(x => x.site.text.toString).getOrElse(missingName), take(K.Colon).map(_ => tpe()), source.span(start, end))
      case Some(Token(K.Underscore, s)) =>
        take(K.Underscore)
        val end = lastBoundary
        Parameter(None, take(K.Identifier).map(x => x.site.text.toString).getOrElse(missingName), take(K.Colon).map(_ => tpe()), source.span(start, end))
      case Some(Token(K.Identifier, s)) => 
        val end = lastBoundary
        Parameter(take(K.Identifier).map(x => x.site.text.toString), take(K.Identifier).map(x => x.site.text.toString).getOrElse(missingName), take(K.Colon).map(_ => tpe()), source.span(start, end))
      case _ => recover(ExpectedTree("declaration", emptySiteAtLastBoundary), ErrorTree.apply)

  /** Parses and returns a type declaration. */
  private[parsing] def typeDeclaration(): TypeDeclaration =
    //ALAMAIN
    val start = lastBoundary
    expect(K.Type)
    val id = identifier().value
    expect(K.Eq)
    val t = tpe()
    TypeDeclaration(id, List.empty, t, source.span(start, lastBoundary))

  /** Parses and returns a list of parameter declarations in angle brackets. */
  //--- This is intentionally left in the handout /*+++ +++*/
  private def typeParameterList(): List[Parameter] =
    inAngles(() => commaSeparatedList(K.RAngle.matches, parameter))
      .collect({ case p: Parameter => p })

  // --- Expressions ----------------------------------------------------------

  /** Parses and returns a term-level expression. */
  def expression(): Expression =
    infixExpression()

  /** Parses and returns an infix expression. */
  private[parsing] def infixExpression(precedence: Int = ast.OperatorPrecedence.min): Expression =
    
    def infixExpressionHelper(lhs: Expression, min_precedence: Int, start : Int): Expression =

      var lhs2 : Expression = lhs
      var continue1 = true

      while (continue1) do
        peek match 
          case Some(value) => 
            if value.kind.isOperatorPart then 
              val backup0 = snapshot()
              var po = operatorIdentifier()._1
              if po == None then continue1 = false
              else
                var p = po.get
                if p.precedence >= min_precedence then 
                  
                  var rhs = ascribed()

                  var continue2 = true
                  //ED : All operators are left associative
                  while (continue2) do 
                    peek match 
                      case Some(value) => 
                        if value.kind.isOperatorPart then 
                          val backup = snapshot()
                          val po2 = operatorIdentifier()._1
                          if po2 == None then continue2 = false
                          else
                            val p2 = po2.get
                            if p2.precedence > p.precedence then 
                              restore(backup)
                              rhs = infixExpressionHelper(rhs, p.precedence + 1, start)
                            else 
                              continue2 = false
                              restore(backup)
                        else continue2 = false
                      case _ => continue2 = false

                  val end = lastBoundary
                  lhs2 = InfixApplication(Identifier(p.toString,source.span(start,end)), lhs2, rhs, source.span(lhs.site.start,rhs.site.end))

                else 
                  continue1 = false
                  restore(backup0)
            else continue1 = false
              
          case None => continue1 = false
      lhs2
    
    infixExpressionHelper(ascribed(), precedence, lastBoundary)
    

  /** Parses and returns an expression with an optional ascription. */
  private[parsing] def ascribed(): Expression =
    //ALAMAIN
    val start = lastBoundary
    val prefix = prefixExpression()
    peek match
      case Some(Token(K.At, _)) =>
        val op = typecast()
        val asc = typeIdentifier()
        AscribedExpression(prefix, op, asc, source.span(start, lastBoundary))
      case Some(Token(K.AtBang, _)) =>
        val op = typecast()
        val asc = typeIdentifier()
        AscribedExpression(prefix, op, asc, source.span(start, lastBoundary))
      case Some(Token(K.AtQuery, _)) =>
        val op = typecast()
        val asc = typeIdentifier()
        AscribedExpression(prefix, op, asc, source.span(start, lastBoundary))
      case _ => prefix

  /** Parses and returns a prefix application. */
  private[parsing] def prefixExpression(): Expression =
    //ALAMAIN
    val start = lastBoundary
    peek match 
      case Some(Token(K.Operator, _)) => 
        val op = operator()
        if !noWhitespaceBeforeNextToken then
          op
        else
          op match
            case Identifier(value,site) =>
              PrefixApplication(Identifier(value,site), compoundExpression(), source.span(start, lastBoundary))
            case _ => op
      case _ => compoundExpression()

  /** Parses and returns a compound expression. */
  private[parsing] def compoundExpression(): Expression =
    //ALAMAIN
    def compoundExpressionAcc(acc : Expression, start : Int): Expression =
      peek match
        case Some(Token(K.Dot, _)) => 
          take()
          peek match 
            case Some(Token(K.Integer, _)) => 
              val selectee = integerLiteral()
              val end = lastBoundary
              compoundExpressionAcc(Selection(acc, selectee, source.span(start, end)), start)
            case Some(Token(K.Identifier, _)) => 
              val selectee = identifier()
              val end = lastBoundary
              compoundExpressionAcc(Selection(acc, selectee, source.span(start, end)), start)
            case Some(Token(K.Operator,_)) =>
              var id = Identifier("",source.span(start, lastBoundary))
              operatorIdentifier() match 
                case (Some(op), site) => id = Identifier(op.toString, site)
                case _ => recover(ExpectedTree("Error! Expected Operator", emptySiteAtLastBoundary), ErrorTree.apply)
              val end = lastBoundary
              compoundExpressionAcc(Selection(acc, id, source.span(start, end)), start)
            case _ => recover(ExpectedTree("expression", emptySiteAtLastBoundary), ErrorTree.apply)

        case Some(Token(K.LParen, _)) =>
          val end = lastBoundary
          compoundExpressionAcc(Application(acc, parenthesizedLabeledList(() => expression()), source.span(start, end)), start)
        case _ => acc

    compoundExpressionAcc(primaryExpression(), lastBoundary)


  /** Parses and returns a term-level primary exression.
   *
   *  primary-expression ::=
   *    | value-identifier
   *    | integer-literal
   *    | float-literal
   *    | string-literal
   */
  private[parsing] def primaryExpression(): Expression =
    peek match
      case Some(Token(K.Identifier, s)) =>
        identifier()
      case Some(Token(K.True, _)) =>
        booleanLiteral()
      case Some(Token(K.False, _)) =>
        booleanLiteral()
      case Some(Token(K.Integer, _)) =>
        integerLiteral()
      case Some(Token(K.Float, _)) =>
        floatLiteral()
      case Some(Token(K.String, _)) =>
        stringLiteral()
      case Some(Token(K.Label, _)) =>
        recordExpression()
      case Some(Token(K.If, _)) =>
        conditional()
      case Some(Token(K.Match, _)) =>
        mtch()
      case Some(Token(K.Let, _)) =>
        let()
      case Some(Token(K.LParen, _)) =>
        lambdaOrParenthesizedExpression()
      case Some(t) if t.kind.isOperatorPart =>
        operator()
      case _ =>
        recover(ExpectedTree("expression", emptySiteAtLastBoundary), ErrorTree.apply)
  
  /** Parses and returns an Boolean literal expression. */
  private[parsing] def booleanLiteral(): BooleanLiteral =
    val s = expect("Boolean literal", K.True | K.False)
    BooleanLiteral(s.site.text.toString, s.site)

  /** Parses and returns an integer literal expression. */
  private[parsing] def integerLiteral(): IntegerLiteral =
    val s = expect(K.Integer)
    IntegerLiteral(s.site.text.toString, s.site)

  /** Parses and returns a floating-point literal expression. */
  private[parsing] def floatLiteral(): FloatLiteral =
    val s = expect(K.Float)
    FloatLiteral(s.site.text.toString, s.site)
    

  /** Parses and returns a string literal expression. */
  private[parsing] def stringLiteral(): StringLiteral =
    val s = expect(K.String)
    StringLiteral(s.site.text.toString, s.site)
    

  /** Parses and returns a term-level record expression. */
  private def recordExpression(): Record =
    //Jsp si on peut faire comme ca
    record(recordExpressionFields, Record.apply)


  /** Parses and returns the fields of a term-level record expression. */
  private def recordExpressionFields(): List[Labeled[Expression]] =
    peek match 
      case Some(Token(K.LParen,_)) => parenthesizedLabeledList(expression)
      case _ => List.empty
    

  /** Parses and returns a conditional expression. */
  private[parsing] def conditional(): Expression =
    //ALAMAIN
    val start = lastBoundary
    expect(K.If)
    val iff = expression()
    expect(K.Then)
    val thenn = expression()
    expect(K.Else)
    val elsee = expression()
    val end = lastBoundary
    Conditional(iff, thenn, elsee, source.span(start, end))


  /** Parses and returns a match expression. */
  private[parsing] def mtch(): Expression =
    //ALAMAIN
    val start = lastBoundary
    expect(K.Match)
    val e = expression()
    val b = matchBody()
    val end = lastBoundary
    Match(e, b, source.span(start, end))

  /** Parses and returns a the cases of a match expression. */
  private def matchBody(): List[Match.Case] =
    @tailrec def loop(partialResult: List[Match.Case]): List[Match.Case] =
      peek match
        case Some(Token(K.RBrace, _)) =>
          partialResult
        case Some(Token(K.Case, _)) =>
          loop(partialResult :+ matchCase())
        case _ =>
          report(ExpectedTokenError(K.Case, emptySiteAtLastBoundary))
          discardUntilRecovery()
          if peek == None then partialResult else loop(partialResult)
    inBraces({ () => recovering(K.Case.matches, () => loop(List())) })

  /** Parses and returns a case in a match expression. */
  private def matchCase(): Match.Case =
    val s = peek.map((t) => t.site)
    if take(K.Case) == None then
      report(ExpectedTokenError(K.Case, emptySiteAtLastBoundary))
    val p = pattern()
    if take(K.Then) == None then
      recover(
        ExpectedTokenError(K.Then, emptySiteAtLastBoundary),
        (e) => Match.Case(p, ErrorTree(e), s.get.extendedTo(lastBoundary)))
    else
      val b = expression()
      Match.Case(p, b, s.get.extendedTo(lastBoundary))

  /** Parses and returns a let expression. */
  private[parsing] def let(): Let =
    //ALAMAIN
    val start = lastBoundary
    //take(K.Let)
    val b = binding(false)
    //TAKE or EXPECT
    take(K.LBrace)
    val e = expression()
    take(K.RBrace)
    val end = lastBoundary
    Let(b, e, source.span(start, end))

  /** Parses and returns a lambda or parenthesized term-level expression. */
  private def lambdaOrParenthesizedExpression(): Expression =
    //ALAMAIN
    val start = lastBoundary
    val snap = snapshot()
    take()
    expression() match
      case ErrorTree(site) => 
        restore(snap)
        val l = valueParameterList()
        peek match 
          case Some(Token(K.LBrace,_)) =>
            take()
            val expr = expression()
            take()
            Lambda(l, None, expr, source.span(start,lastBoundary))
          case Some(Token(K.Arrow, _)) =>
            take()
            val tp = tpe()
            peek match
              case Some(Token(K.LBrace,_)) =>
                take()
                val expr = expression()
                take()
                Lambda(l,Some(tp), expr, source.span(start, lastBoundary))
              case _ => 
                recover(ExpectedTree("lamda", emptySiteAtLastBoundary), ErrorTree.apply)
          case _ =>
            recover(ExpectedTree("lamda", emptySiteAtLastBoundary), ErrorTree.apply)
      case _ =>
        restore(snap)
        take()
        val expr = expression()
        take()
        ParenthesizedExpression(expr, source.span(start, lastBoundary))

  /** Parses and returns an operator. */
  private def operator(): Expression =
    operatorIdentifier() match
      case (Some(o), p) => Identifier(p.text.toString, p)
      case (_, p) => ErrorTree(p)

  /** Parses and returns an operator identifier, along with its source positions.
   *
   *  If the the parsed operator is undefined, a diagnostic is reported and the returned identifier
   *  is `None`. In any case, the returned span represents the positions of the parsed identifier.
   */
  private def operatorIdentifier(): (Option[ast.OperatorIdentifier], SourceSpan) =
    import ast.OperatorIdentifier as O

    @tailrec def loop(start: Int, end: Int): (Option[ast.OperatorIdentifier], SourceSpan) =
      if takeIf((t) => t.isOperatorPartImmediatelyAfter(end)) != None then
        loop(start, lastBoundary)
      else
        val p = source.span(start, end)
        val s = p.text
        val o = if s == "||" then
          Some(O.LogicalOr)
        else if s == "&&" then
          Some(O.LogicalAnd)
        else if s == "<" then
          Some(O.LessThan)
        else if s == "<=" then
          Some(O.LessThanOrEqual)
        else if s == ">" then
          Some(O.GreaterThan)
        else if s == ">=" then
          Some(O.GreaterThanOrEqual)
        else if s == "==" then
          Some(O.Equal)
        else if s == "!=" then
          Some(O.NotEqual)
        else if s == "..." then
          Some(O.ClosedRange)
        else if s == "..<" then
          Some(O.HaflOpenRange)
        else if s == "+" then
          Some(O.Plus)
        else if s == "-" then
          Some(O.Minus)
        else if s == "|" then
          Some(O.BitwiseOr)
        else if s == "^" then
          Some(O.BitwiseXor)
        else if s == "*" then
          Some(O.Star)
        else if s == "/" then
          Some(O.Slash)
        else if s == "%" then
          Some(O.Percent)
        else if s == "&" then
          Some(O.Ampersand)
        else if s == "<<" then
          Some(O.LeftShift)
        else if s == ">>" then
          Some(O.RightShift)
        else if s == "~" then
          Some(O.Tilde)
        else if s == "!" then
          Some(O.Bang)
        else
          report(SyntaxError(s"undefined operator '${s}'", p))
          None
        (o, p)

    val h = expect("operator", (t) => t.kind.isOperatorPart)
    loop(h.site.start, h.site.end)

  /** Parses and returns a type cast operator. */
  private def typecast(): Typecast =
    peek match
      case Some(Token(K.At, _)) =>
        take(); Typecast.Widen
      case Some(Token(K.AtQuery, _)) =>
        take(); Typecast.Narrow
      case Some(Token(K.AtBang, _)) =>
        take(); Typecast.NarrowUnconditionally
      case _ =>
        throw FatalError("expected typecast operator", emptySiteAtLastBoundary)

  // --- Types ----------------------------------------------------------------

  /** Parses and returns a type-level expression. */
  private[parsing] def tpe(): Type =
    //ALAMAIN
    val start = lastBoundary
    var t = primaryType()
    var list = List[Type]()
    list = list ::: List(t)
    var continue = true
    var count = 0

    while continue do
      peek match
        case Some(Token(K.Operator,_)) =>
          operator() match
            case Identifier(value, site) => 
              if (value == "|") then
                val t2 = primaryType()
                list = list ::: List(t2)
                count = count + 1
              else
                continue = false
            case _ => continue = false
          
        case _ => continue = false
      
    val end = lastBoundary
    if count > 0 then
      Sum(list, source.span(start, end))
    else
      t

  /** Parses and returns a type-level primary exression. */
  private def primaryType(): Type =
    peek match
      case Some(Token(K.Identifier, s)) =>
        typeIdentifier()
      case Some(Token(K.Label, _)) =>
        recordType()
      case Some(Token(K.LParen, _)) =>
        arrowOrParenthesizedType()
      case _ =>
        recover(ExpectedTree("type expression", emptySiteAtLastBoundary), ErrorTree.apply)

  /** Parses and returns a type identifier. */
  private def typeIdentifier(): Type =
    //ALAMAIN
    val start = lastBoundary
    take(K.Identifier) match
      case Some(value) => 
        val end = lastBoundary
        TypeIdentifier(value.site.text.toString, source.span(start, end))
      case _ => recover(ExpectedTree("Error! Expected : Type Identifier", emptySiteAtLastBoundary), ErrorTree.apply)

  /** Parses and returns a list of type arguments. */
  private def typeArguments(): List[Labeled[Type]] =
    parenthesizedLabeledList(tpe)

  /** Parses and returns a type-level record expressions. */
  private[parsing] def recordType(): RecordType =
    record(recordTypeFields, RecordType.apply)

  /** Parses and returns the fields of a type-level record expression. */
  private def recordTypeFields(): List[Labeled[Type]] =
    peek match
      case Some(Token(K.LParen, _)) =>
        parenthesizedLabeledList(tpe)
      case _ =>
        List.empty

  /** Parses and returns a arrow or parenthesized type-level expression. */
  private[parsing] def arrowOrParenthesizedType(): Type =
    val start = lastBoundary 
    var lab = parenthesizedLabeledList( () => tpe() )
    
    peek match
      case None => 
        return ParenthesizedType(lab.head.value, source.span(start, lastBoundary))
      case Some(Token(K.Arrow,_)) => 
        take()
        return Arrow(lab, tpe(),source.span(start, lastBoundary))
      case _ =>  recover(ExpectedTree("not arrow/parethisizertype", emptySiteAtLastBoundary), ErrorTree.apply)

  // --- Patterns -------------------------------------------------------------

  /** Parses and returns a pattern. */
  private[parsing] def pattern(): Pattern =
    peek match
      case Some(Token(K.Underscore, _)) =>
        wildcard()
      case Some(Token(K.Label, _)) =>
        recordPattern()
      case Some(Token(K.Let, _)) =>
        bindingPattern()
      case _ =>
        valuePattern()

  /** Parses and returns a wildcard pattern. */
  def wildcard(): Wildcard =
    //ALAMAIN
    val start = lastBoundary
    val s = expect(K.Underscore)
    val end = lastBoundary
    Wildcard(source.span(start, end))

  /** Parses and returns a record pattern. */
  private def recordPattern(): RecordPattern =
    //ALAMAIN
    record(recordPatternFields, RecordPattern.apply)

  /** Parses and returns the fields of a record pattern. */
  private def recordPatternFields(): List[Labeled[Pattern]] =
    peek match
      case Some(Token(K.LParen,_)) =>
        parenthesizedLabeledList(pattern)
      case _ =>
        List()

  /** Parses and returns a binding pattern. */
  private def bindingPattern(): Binding =
    //ALAMAIN
    val bin = binding(false)
    if bin.initializer != None then
      report(SyntaxError("initializer not allowed in binding pattern", bin.site))
      bin
    else
      bin

  /** Parses and returns a value pattern. */
  private def valuePattern(): ValuePattern =
    val start = lastBoundary
    val exp = expression()
    val end = lastBoundary
    ValuePattern(exp, source.span(start, end))

  // --- Common trees ---------------------------------------------------------

  /** Parses and returns an identifier. */
  private def identifier(): Identifier =
    val s = expect(K.Identifier)
    Identifier(s.site.text.toString, s.site)

  // --- Combinators ----------------------------------------------------------

  /** Parses and returns a record.
   *
   *  @param fields A closure parsing the fields of the record.
   *  @param make A closure constructing a record tree from its name, fields, and site.
   */
  private def record[Field <: Labeled[Tree], T <: RecordPrototype[Field]](
      fields: () => List[Field],
      make: (String, List[Field], SourceSpan) => T
  ): T =
    val start = lastBoundary
    val v1 = expect(K.Label).site.text.toString
    val end = lastBoundary
    make(v1, fields(), source.span(start, end))
    

  /** Parses and returns a parenthesized list of labeled value.
   *
   *  See also [[this.labeledList]].
   *
   *  @param value A closure parsing a labeled value.
   */
  private[parsing] def parenthesizedLabeledList[T <: Tree](
      value: () => T
  ): List[Labeled[T]] =
    //ALAMAIN
    inParentheses(() => commaSeparatedList(K.RParen.matches, () => labeled(value)))

  /** Parses and returns a value optionally prefixed by a label.
   *
   *  This combinator attempts to parse a label `n` followed by a colon and then applies `value`.
   *  If that succeeds, returned tree is the result of `value` labeled by `n`. If there is no label,
   *  the combinator backtracks, re-applies `value`, and returns its result sans label.
   *
   *  @param value A closure parsing a labeled value.
   */
  private[parsing] def labeled[T <: Tree](
      value: () => T
  ): Labeled[T] =
    //ALAMAIN
    val s = snapshot()
    val start = s.lastBoundary
    peek match
      case Some(Token(K.Identifier,_)) =>
        val lab = identifier()
        peek match
          case Some(Token(K.Colon,_)) =>
            take(K.Colon)
            val va = value()
            return ast.Labeled[T](Some(lab.value), va, source.span(s.lastBoundary,lastBoundary))
          case _ => 
            restore(s)
            val va = value()
            Labeled(None, va, source.span(s.lastBoundary,lastBoundary))
      case Some(Token(k,_))=>
        if(k.isKeyword) then
          val lab = take()
          peek match
            case Some(Token(K.Colon,_)) =>
              take(K.Colon)
              val va = value()
              return ast.Labeled[T](Some(lab.get.kind.toString.toLowerCase()), va, source.span(s.lastBoundary,lastBoundary))
            case _ => 
              restore(s)
              val va = value()
              Labeled(None, va, source.span(s.lastBoundary,lastBoundary))
        else 
          restore(s)
          val va = value()
          Labeled(None, va, source.span(s.lastBoundary,lastBoundary))
      case _ => 
        //will never happen
        return ast.Labeled[T](None, value(), source.span(s.lastBoundary,lastBoundary))

  /** Parses and returns a sequence of `element` separated by commas and delimited on the RHS  by a
   *  token satisfying `isTerminator`.
   */
  private[parsing] def commaSeparatedList[T](isTerminator: Token => Boolean, element: () => T): List[T] =
    @tailrec def loop(partialResult: List[T]): List[T] =
      if peek.map(isTerminator).getOrElse(false) then
        partialResult
      else
        val nextPartialResult = partialResult :+ recovering(K.Comma.matches, element)
        if peek.map(isTerminator).getOrElse(false) then
          nextPartialResult
        else if take(K.Comma) != None then
          loop(nextPartialResult)
        else
          report(ExpectedTokenError(K.Comma, emptySiteAtLastBoundary))
          loop(nextPartialResult)
    loop(List())

  /** Parses and returns `element` surrounded by a pair of parentheses. */
  private[parsing] def inParentheses[T](element: () => T): T =
    //ALAMAIN
    delimited(K.LParen, K.RParen, element)
    

  /** Parses and returns `element` surrounded by a pair of braces. */
  private[parsing] def inBraces[T](element: () => T): T =
    //ALAMAIN
    delimited(K.LBrace, K.RBrace, element)

  /** Parses and returns `element` surrounded by angle brackets. */
  private[parsing] def inAngles[T](element: () => T): T =
    //ALAMAIN
    delimited(K.LAngle, K.RAngle, element)

  /** Parses and returns `element` surrounded by a `left` and `right`. */
  private[parsing] def delimited[T](left: Token.Kind, right: Token.Kind, element: () => T): T =
    if take(left) == None then
      report(ExpectedTokenError(right, emptySiteAtLastBoundary))
    val contents = recovering(right.matches, element)
    if take(right) == None then
      report(ExpectedTokenError(right, emptySiteAtLastBoundary))
    contents

  /** Returns the result of `element` with `isRecoveryToken` added to the recovery predicates. */
  private def recovering[T](isRecoveryToken: Token => Boolean, element: () => T): T =
    recoveryPredicates += isRecoveryToken
    try
      element()
    finally
      recoveryPredicates.dropRightInPlace(1)

  // --- Utilities ------------------------------------------------------------

  /** Returns `true` iff there isn't any whitespace before the next token in the stream. */
  private def noWhitespaceBeforeNextToken: Boolean =
    peek.map((t) => lastBoundary == t.site.start).getOrElse(false)

  /** Reports a missing identifier and returns "_". */
  def missingName =
    report(ExpectedTokenError(K.Identifier, emptySiteAtLastBoundary))
    "_"

  /** Reports `error`, advances the stream to the next recovery token, and returns the result of
   *  calling `errorTree` on the skipped positions.
   */
  private def recover[T](error: SyntaxError, errorTree: SourceSpan => T): T =
    report(error)
    errorTree(discardUntilRecovery())

  /** Advances the stream to the next recovery token and returns the skipped positions. */
  private def discardUntilRecovery(): SourceSpan =
    @tailrec def loop(s: Int): SourceSpan =
      if !peek.isDefined || Reverse(recoveryPredicates).exists((p) => p(peek.get)) then
        source.span(s, lastBoundary)
      else
        take()
        loop(s)
    loop(lastBoundary)

  /** Consumes and returns the next token in the stream iff it has kind `k` or throw an exception
    * otherwise,
    */
  private def expect(construct: String, predicate: (Token) => Boolean): Token =
    takeIf(predicate) match
      case Some(next) => next
      case _ => throw FatalError(s"expected ${construct}", emptySiteAtLastBoundary)

  /** Consumes and returns the next token in the stream iff it has kind `k` or throw an exception
    * otherwise,
    */
  private def expect(k: Token.Kind): Token =
    take(k) match
      case Some(next) => next
      case _ => throw FatalError(s"expected ${k}", emptySiteAtLastBoundary)

  /** Returns the next token in the stream without consuming it. */
  private def peek: Option[Token] =
    if lookahead == None then
      lookahead = tokens.next()
    lookahead

  /** Consumes the next token in the stream iff it has kind `k` and returns the result of `action`
   *  applied on that token. */
  private def taking[T](k: Token.Kind, action: Token => T): Option[T] =
    take(k).map(action)

  /** Consumes and returns the next token in the stream. */
  private def take(): Option[Token] =
    peek.map({ (next) =>
      lastBoundary = next.site.end
      lookahead = None
      next
    })

  /** Consumes and returns the next token in the stream iff it has kind `k`. */
  private def take(k: Token.Kind): Option[Token] =
    takeIf(k.matches)

  /** Consumes and returns the next character in the stream iff it satisfies `predicate`. */
  private def takeIf(predicate: Token => Boolean): Option[Token] =
    if peek.map(predicate).getOrElse(false) then take() else None

  /** Returns an empty range at the position of the last consumed token. */
  private def emptySiteAtLastBoundary: SourceSpan =
    source.span(lastBoundary, lastBoundary)

  /** Reports the given diagnostic. */
  private def report(d: SyntaxError): Unit =
    errors += d

  /** Returns a backup of this instance's state. */
  private[parsing] def snapshot(): Parser.Snapshot =
    Parser.Snapshot(
      tokens.copy(), lastBoundary, lookahead, errors.length, recoveryPredicates.length)

  /** Restores this instance to state `s`. */
  private[parsing] def restore(s: Parser.Snapshot): Unit =
    tokens = s.tokens
    lastBoundary = s.lastBoundary
    lookahead = s.lookahead
    errors.dropRightInPlace(errors.length - s.errorCount)
    recoveryPredicates.dropRightInPlace(recoveryPredicates.length - s.recoveryPredicateCount)

end Parser

object Parser:

  /** The information necessary to restore the state of a parser instance. */
  private[parsing] final case class Snapshot(
      tokens: Lexer,
      lastBoundary: Int,
      lookahead: Option[Token],
      errorCount: Int,
      recoveryPredicateCount: Int)

end Parser

extension (self: Token.Kind) def | (other: Token.Kind): (Token) => Boolean =
  (t) => (t.kind == self) || (t.kind == other)

extension (self: Token => Boolean) def | (other: Token.Kind): (Token) => Boolean =
  (t) => self(t) || (t.kind == other)