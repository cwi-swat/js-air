@license{
Copyright (c) 2017, Bert Lisser - Centrum Wiskunde & Informatica (CWI)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.  
}
@contributor{Bert Lisser - CWI}
module lang::javascript::m3::AST

extend analysis::m3::AST;

data Declaration
  = program(list[Statement] stats)
  ;

data Statement
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
  | \switch(Expression discriminant, list[Statement] cases)
  | switchCase(Expression \test, list[Statement] consequents)
  | switchCase(list[Statement] consequents)
  | \return(Expression argument)
  | \return()
  | \throw(Expression argument)
  | \try(list[Statement] block, Statement handler, list[Statement] finalizer)
  | \try(list[Statement] block, list[Statement] finalizer) 
  | \try(list[Statement] block, Statement handler)
  | catchClause(Expression pattern, Expression guard, list[Statement] statBody)
  | catchClause(Expression pattern, list[Statement] statBody) 
  | \while(Expression \test, Statement body)
  | doWhile(Statement body, Expression \test)
  | \for(Statement init, Expression condition, Statement body, Expression update) // exps contains test, update
  | forIn(Statement varDecls, str kind, Expression right, Statement body)
  | forIn(Expression left, Expression right, Statement body)
  | forOf(Statement varDecls, str kind, Expression right, Statement body)
  | forOf(Expression left, Expression right, Statement body)
  | let(Statement varDecls, Statement body)
  | debugger()
  | functionDecl(str id, list[Expression] params, list[Statement] statBody, bool generator)
  | varDecl(lrel[Expression id, Expression init] declarations, str kind)
  | varDecl(lrel[Expression id, Expression init] declarations)
  ;

data Expression
  = this()
  | array(list[Expression] elements)
  | object(lrel[Expression key, Expression \value, str kind] properties)
  | object(lrel[Expression key, Expression pattern] patternProperties) 
  | function(str id, list[Expression] params, list[Statement] statBody, bool generator)
  | sequence(list[Expression] expressions)
  | min(Expression argument) 
  | plus(Expression argument) 
  | not(Expression argument) 
  | bitNot(Expression argument)
  | typeOf(Expression argument) 
  | \void(Expression argument) 
  | delete(Expression argument)
  | equals(Expression lhs, Expression rhs) 
  | notEquals(Expression lhs, Expression rhs) 
  | longEquals(Expression lhs, Expression rhs) 
  | longNotEquals(Expression lhs, Expression rhs)
  | lt(Expression lhs, Expression rhs) 
  | leq(Expression lhs, Expression rhs) 
  | gt(Expression lhs, Expression rhs) 
  | geq(Expression lhs, Expression rhs)
  | shiftLeft(Expression lhs, Expression rhs) 
  | shiftRight(Expression lhs, Expression rhs) 
  | longShiftRight(Expression lhs, Expression rhs)
  | plus(Expression lhs, Expression rhs) 
  | min(Expression lhs, Expression rhs) 
  | times(Expression lhs, Expression rhs) 
  | div(Expression lhs, Expression rhs) 
  | rem(Expression lhs, Expression rhs)
  | bitOr(Expression lhs, Expression rhs) 
  | bitXor(Expression lhs, Expression rhs) 
  | bitAnd(Expression lhs, Expression rhs) 
  | \in(Expression lhs, Expression rhs) 
  | comma(Expression lhs, Expression rhs)
  | instanceOf(Expression lhs, Expression rhs) 
  | range(Expression lhs, Expression rhs)
  | assign(Expression lhs, Expression rhs) 
  | plusAssign(Expression lhs, Expression rhs) 
  | minAssign(Expression lhs, Expression rhs) 
  | timesAssign(Expression lhs, Expression rhs) 
  | divAssign(Expression lhs, Expression rhs) 
  | remAssign(Expression lhs, Expression rhs)
  | shiftLeftAssign(Expression lhs, Expression rhs) 
  | shiftRightAssign(Expression lhs, Expression rhs) 
  | longShiftRightAssign(Expression lhs, Expression rhs)
  | bitOrAssign(Expression lhs, Expression rhs) 
  | bitXorAssign(Expression lhs, Expression rhs) 
  | bitAndAssign(Expression lhs, Expression rhs)
  | updatePreDec(Expression argument)
  | updatePreInc(Expression argument)
  | updatePostDec(Expression argument)
  | updatePostInc(Expression argument)
  | or(Expression lhs,  Expression rhs) 
  | and(Expression lhs,  Expression rhs)
  | conditional(Expression \test, Expression consexp, Expression altexp) // TODO: inline expressions
  | new(Expression callee, list[Expression] arguments)
  | call(Expression callee, list[Expression] arguments)
  | member(Expression object, str strProperty)
  | member(Expression object, Expression expProperty)
  | yield(Expression argument)
  | yield()
  | comprehension(Expression expBody, list[Expression] comprehensionBlocks, Expression \filter)
  | comprehension(Expression expBody, list[Expression] comprehensionBlocks)
  | generator(Expression expBody, list[Expression] comprehensionBlocks, Expression \filter)
  | generator(Expression expBody, list[Expression] comprehensionBlocks)
  | comprehensionBlock(Expression pattern, Expression right, bool each = false)
  | graph(int index, Expression expression)
  | graphIndex(int index)
  | let(lrel[Expression id, Expression init] inits, Expression expBody)
  | variable(str name)
  | id(str name)
  | undefined()
  | string(str strValue)
  | boolean(bool boolValue)
  | null() 
  | number(num numValue)
  | regExp(str regexp)
  ;

