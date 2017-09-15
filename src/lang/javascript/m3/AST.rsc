module lang::javascript::m3::AST

extend analysis::m3::AST;

data Program(loc \loc=|unknown:///|)
  = program(list[Statement] stats)
  ;

data ForInit(loc \loc=|unknown:///|)
  = varDecl(list[VariableDeclarator] declarations, str kind)
  | expression(Expression exp)
  | none()
  ;

data Init(loc \loc=|unknown:///|) 
  = expression(Expression exp)
  | none()
  ;

data Statement(loc \loc=|unknown:///|)
  = empty()
  | block(list[Statement] stats)
  | block(Statement stat)
  | expression(Expression exp)
  | \if(Expression \test, Statement consequent, Statement alternate)
  | \if(Expression \test, Statement consequent)
  | labeled(str label, Statement stat)
  | \break()
  | \continue()
  | \break(str label)
  | \continue(str label)
  | with(Expression object, Statement stat)
  | \switch(Expression discriminant, list[SwitchCase] cases)
  | \return(Expression argument)
  | \return()
  | \throw(Expression argument)
  | \try(list[Statement] block, CatchClause handler, list[Statement] finalizer)
  | \try(list[Statement] block, list[Statement] finalizer) // Bert
  | \try(list[Statement] block, CatchClause handler)
  // | \try(list[Statement] block, CatchClause handler, list[CatchClause] guardedHandlers, list[Statement] finalizer)
 // | \try(list[Statement] block, list[CatchClause] guardedHandlers, list[Statement] finalizer)
 //  | \try(list[Statement] block, CatchClause handler, list[CatchClause] guardedHandlers)
 //  | \try(list[Statement] block, list[CatchClause] guardedHandlers)
  | \while(Expression \test, Statement body)
  | doWhile(Statement body, Expression \test)
  | \for(ForInit init, Expression condition, Statement body, Expression update) // exps contains test, update
  | forIn(list[VariableDeclarator] decls, str kind, Expression right,Statement body)
  | forIn(Expression left, Expression right, Statement body)
  | forOf(list[VariableDeclarator] decls, str kind, Expression right, Statement body)
  | forOf(Expression left, Expression right, Statement body)
  | let(list[tuple[Pattern id, Init init]] inits, Statement body)
  | debugger()
  | functionDecl(str id, list[Identifier] params, list[Statement] statBody, bool generator)
 //  | functionDecl(str id, list[Pattern] params, list[Expression] defaults, str rest, Expression expBody, bool generator)
  | varDecl(list[VariableDeclarator] declarations, str kind)
  ;



data VariableDeclarator(loc \loc=|unknown:///|)
  = variableDeclarator(Identifier id, Init init)
  ;

data LitOrId(loc \loc=|unknown:///|)
  = id(str name)
  | lit(Literal \value)
  ;

data Expression(loc \loc=|unknown:///|)
  = this()
  | array(list[Expression] elements)
  | object(list[tuple[LitOrId key, Expression \value, str kind]]
properties)
  | function(str id, list[Identifier] params, list[Statement] statBody, bool generator)
            //bool generator = false)
  //| function(str name, // "" = null
  //          list[Pattern] params,
  //          list[Expression] defaults,
  //          str rest, // "" = null
  //          Expression expBody)
            //,
            //bool generator = false)
  //| arrow(list[Pattern] params,
  //list[Expression] defaults,
  //          str rest, // "" = null
  //          list[Statement] statBody)
  //| arrow(list[Pattern] params,
  //list[Expression] defaults,
  //          str rest, // "" = null
  //          Expression expBody)
  | sequence(list[Expression] expressions)
  | unary(UnaryOperator operator, Expression argument, bool prefix) // TODO: inline expressions
  | binaryExpression(Expression left, BinaryOperator binaryOp, Expression right) // TODO: inline expressions
  | assignment(Expression left, AssignmentOperator assignOp,  Expression right) // TODO: inline expressions
  | update(UpdateOperator updateOp, Expression argument, bool prefix)
  | logical(Expression left, LogicalOperator logicalOp,  Expression right) // TODO: inline expressions
  | conditional(Expression \test, Expression consequent, Expression alternate) // TODO: inline expressions
  | new(Expression callee, list[Expression] arguments)
  | call(Expression callee, list[Expression] arguments)
  | member(Expression object, str strProperty)
  | member(Expression object, Expression expProperty)
  | yield(Expression argument)
  | yield()
  | comprehension(Expression expBody, list[ComprehensionBlock] blocks,
Expression \filter)
  | comprehension(Expression expBody, list[ComprehensionBlock] blocks)
  | generator(Expression expBody, list[ComprehensionBlock] blocks,
Expression \filter)
  | generator(Expression expBody, list[ComprehensionBlock] blocks)
  | graph(int index, Literal expression)
  | graphIndex(int index)
  | let(list[tuple[Pattern id, Init init]] inits, Expression expBody)
  // not in Spidermonkey's AST API?
  | variable(str name)
  | literal(Literal lit)
  | undefined()
  ;

data Pattern(loc \loc=|unknown:///|)
  = object(list[tuple[LitOrId key, Pattern \value]] properties)
  | array(list[Pattern] elements) // elts contain null!
  | variable(str name)
  ;

data SwitchCase(loc \loc=|unknown:///|)
  = switchCase(Expression \test, list[Statement] consequent)
  | switchCase(list[Statement] consequent)
  ;

data CatchClause(loc \loc=|unknown:///|)
  = catchClause(Pattern param, Expression guard, list[Statement] statBody)
// blockstatement
  | catchClause(Pattern param, list[Statement] statBody) // blockstatement
  ;

data ComprehensionBlock(loc \loc=|unknown:///|)
  = comprehensionBlock(Pattern left, Expression right, bool each = false); // kwparameters should not be used as syntax

data Literal(loc \loc=|unknown:///|)
  = string(str strValue)
  | boolean(bool boolValue)
  | null()
  | number(num numValue)
  | regExp(str regexp)
  ;
  
data Identifier(loc \loc=|unknown:///|) = identifier(str strValue);     

data UnaryOperator(loc \loc=|unknown:///|)
  = min() | plus() | not() | bitNot() | typeOf() | \void() | delete();

data BinaryOperator(loc \loc=|unknown:///|) // TODO: inline expressions
  = equals() | notEquals() | longEquals() | longNotEquals()
  | lt() | leq() | gt() | geq()
  | shiftLeft() | shiftRight() | longShiftRight()
  | plus() | min() | times() | div() | rem()
  | bitOr() | bitXor() | bitAnd() | \in() | comma()
  | instanceOf() | range()
  ;

data LogicalOperator(loc \loc=|unknown:///|)
  = or() | and()
  ;

data AssignmentOperator(loc \loc=|unknown:///|)
  = assign() | plusAssign() | minAssign() | timesAssign() | divAssign() |
remAssign()
  | shiftLeftAssign() | shiftRightAssign() | longShiftRightAssign()
  | bitOrAssign() | bitXorAssign() | bitAndAssign();

data UpdateOperator(loc \loc=|unknown:///|)
  = inc() | dec()
  ;

data ErrorNode = errorNode(str error);

