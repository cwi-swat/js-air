@license{
Copyright (c) 2017, Bert Lisser - Centrum Wiskunde & Informatica (CWI)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.  
}
@contributor{Bert Lisser - CWI}
module lang::javascript::m3::Parser

extend lang::javascript::m3::AST;

import IO;
import Set;
import String;
import List;
import Node;
import lang::json::IO;
import util::ShellExec;
import ValueIO;

list[int] line2Offset = [];

Program parseJavascript(loc js) {
   code = readFile(js);
   line2Offset = [0] + [i | i <- findAll(code, "\n")];
   str src = _parse(js, code);
   node json = parseJSON(#node, src, implicitConstructors=true, implicitNodes=true);
   println(json);
   return buildProgram(json);
}

loc L(node n) {
    if (n.\loc? && node location:= n.\loc && str src :=location.source && node \start := location.\start
          && node end:=location.end && int line1:=\start.line && int column1:=\start.column
          && int line2:=\end.line && int column2:=\end.column
       )  {
         column1 += 1;
         column2 += 1;
         println("col1 <column1> col2 <column2>");
         offset = line2Offset[line1-1] + column1;
         endOffset = line2Offset[line2-1] + column2;
         len = endOffset - offset;
         println("line1 <line1> offsetl1 <line2Offset[line1]>, line2 <line2> offsetl2 <line2Offset[line2]>, offset <offset>, len <len>");
       return str2loc(src)(offset, len, <line1, column1>, <line2, column2>);
    }
    
    return |file:///|; 
}

private loc str2loc(str s)
  = readTextValueString(#loc, "|<s>|");

Program build(node file) {
   if (node program:=file.program) 
     return buildProgram(program);
   // TODO: what else?
}

Program buildProgram(node _program) {
   if (node _body:=_program && list[node] statements:=_body.body)
      return program([buildStatement(statement)|statement<-statements], \loc=L(_program));
   // TODO: no return?
}
    

Statement buildStatement(node _statement) {
    switch(_statement.\type)  {
     case "VariableDeclaration": 
      if (list[node] variableDeclarators:=_statement.declarations) {
         if (str kind:= ((_statement.kind?)?_statement.kind:"var"))
        return 
         Statement::varDecl([buildVariableDeclarator(variableDeclarator)|node variableDeclarator<-variableDeclarators]
            , kind, \loc=L(_statement));
            }
     case "FunctionDeclaration":
         if (node id := _statement.id && list[node] params := _statement.params && node body :=  _statement.body
             && bool generator := _statement.generator && list[node] stats:=body.body && str name := id.name) {
               return functionDecl(name, [buildIdentifier(v)|node v<-params], [buildStatement(stat)|stat<-stats], generator, \loc=L(_statement));
               }
     case "ExpressionStatement": 
        if (node _expression:=_statement.expression)  return Statement::expression(buildExpression(_expression), \loc=L(_statement));
     case "EmptyStatement": return Statement::empty();
     case "BlockStatement": {
      if (_statement.body?)
      if (list[node] stats:=_statement.body)
        return 
         Statement::block([buildStatement(stat)|stat<-stats], \loc=L(_statement)); 
     // labeled(str label, Statement stat)
        if (_statement.block? && node block:=_statement.block) 
            return Statement::block(buildStatement(block)); 
        }
     case "LabeledStatement":
         if (value label:=_statement.label && node body:= _statement.body) {
               if  (node lbl:=label && str name := lbl.name) return labeled(name, buildStatement(body));
               if  (str name:=label) return labeled(name, buildStatement(body), \loc=L(_statement));
               }
     case "IfStatement":  
        if (node condition:=_statement.\test && node consequent:=_statement.consequent)
          if (_statement.alternate?) {
               if (node alternate :=_statement.alternate)
                  return Statement::\if(buildExpression(condition), buildStatement(consequent), buildStatement(alternate), \loc=L(_statement));
            }
          else 
            return Statement::\if(buildExpression(condition), buildStatement(consequent)); 
            // \switch(Expression discriminant, list[SwitchCase] cases)
     case "SwitchStatement": 
          if (node discriminant:= _statement.discriminant && list[node] cases:= _statement.cases)
                return \switch(buildExpression(discriminant), [buildSwitchCase(\case)|node \case<-cases], \loc=L(_statement));
     case "ForStatement": {
        ForInit forInit = ForInit::none(); 
        if (_statement.init? && node init := _statement.init) {
              if (init.\type=="VariableDeclaration" && list[node] variableDeclarators:= init.declarations 
                     && str kind:= init.kind) {
              forInit = 
              ForInit::varDecl([buildVariableDeclarator(variableDeclarator)|node variableDeclarator<-variableDeclarators], kind, \loc=L(_statement));
              }
              else forInit = ForInit::expression(buildExpression(init), \loc=L(_statement));
              } 
        Expression condition = undefined();
        if (_statement.\test? && node _condition := _statement.\test) {
           condition = buildExpression(_condition);
           }
        Expression update = undefined();
        if (_statement.\update? && node _update := _statement.\update) {
           update = buildExpression(_update);
           }
        if (node body := _statement.body) {
           return \for(forInit, condition, buildStatement(body), update, \loc=L(_statement));
           }  
        }
     case "ForInStatement": {
        if (node _right := _statement.right && node body := _statement.body)
        if (node _left := _statement.left) {
              if (_left.\type=="VariableDeclaration" && list[node] variableDeclarators:= _left.declarations 
                     && str kind:= _left.kind) {
                return forIn([buildVariableDeclarator(variableDeclarator)|node variableDeclarator<-variableDeclarators], kind 
                ,buildExpression(_right), buildStatement(body));
              }
            return forIn(buildExpression(_left),buildExpression(_right),buildStatement(body), \loc=L(_statement));
            } 
        }
     case "WhileStatement": {
        if( node condition := _statement.\test && node body:=_statement.body)
           return \while(buildExpression(condition), buildStatement(body), \loc=L(_statement));
        }
     case "DoWhileStatement": {
        if( node condition := _statement.\test && node body:=_statement.body)
           return doWhile(buildStatement(body), buildExpression(condition), \loc=L(_statement));
        }  
     case "ReturnStatement": {
        if (_statement.argument? && node argument := _statement.argument)
          return \return(buildExpression(argument), \loc=L(_statement));
        return(\return());
        }
     case "BreakStatement": {
        if (_statement.label? && node label := _statement.label && str name:=label.name)
          return \break(name);
        return(\break());
        }
      case "ContinueStatement": {
        if (_statement.label? && node label := _statement.label && str name:=label.name)
          return \continue(name, \loc=L(_statement));
        return(\continue());
        }
       case "ThrowStatement": {
        if (node argument := _statement.argument)
          return \throw(buildExpression(argument), \loc=L(_statement));
        }
        case "TryStatement": {
            if (node block:= _statement.block && list[node] stats := block.body) 
                if (_statement.handler? && node handler := _statement.handler) {
                    if (_statement.finalizer? && node finalizer:=_statement.finalizer 
                       && list[node] stats1:=finalizer.body)
                      return \try([buildStatement(stat)|stat<-stats], buildCatchClause(handler), 
                       [buildStatement(stat)|stat<-stats1] );
                 return \try([buildStatement(stat)|stat<-stats], buildCatchClause(handler), \loc=L(_statement));
                 } else 
                   if (_statement.finalizer? && node finalizer:=_statement.finalizer 
                       && list[node] stats1:=finalizer.body)
                   {
                   return \try([buildStatement(stat)|stat<-stats], [buildStatement(stat)|stat<-stats1], \loc=L(_statement) );
                   }            
            }
      }
}
    
    
VariableDeclarator buildVariableDeclarator(node _variableDeclarator) {
    if (node id:=_variableDeclarator.id)
       return variableDeclarator(buildIdentifier(id), buildInit(_variableDeclarator), \loc=L(_variableDeclarator));
    // TODO: no return? 
}
    
Identifier buildIdentifier(node id) {
    if (str name:=id.name)
        return identifier(name, \loc=L(id));
}
    
Init buildInit(node _variableDeclarator) {
    if (_variableDeclarator.init? && node init:=_variableDeclarator.init) return Init::expression(buildExpression(init));
        return Init::none();
}

SwitchCase buildSwitchCase(node _switchCase) {
  if (list[node] stats:= _switchCase.consequent) {
     if (_switchCase.\test? && node condition:=_switchCase.\test) 
        return switchCase(buildExpression(condition), 
        [buildStatement(stat)|stat<-stats]);  
     return switchCase([buildStatement(stat)|stat<-stats], \loc=L(_switchCase));
     }
  // TODO: no return?       
}

CatchClause buildCatchClause(node _catchClause) {
    if (node param:=_catchClause.param && node _body:=_catchClause.body 
           && list[node] stats := _body.body && str name:=param.name) 
        return catchClause(Pattern::variable(name), [buildStatement(stat)|stat<-stats], \loc=L(_catchClause));
    // TODO no return?      
}
    
tuple[LitOrId key, Expression \value, str kind]  buildObjectProperty(node _objectProperty) {
    if (node key:=_objectProperty.key && node val := _objectProperty.\value) {
        if (key.\type=="Identifier" && str name := key.name)
            return <LitOrId::id(name), buildExpression(val), "">; 
        if ((key.\type=="StringLiteral"|| key.\type=="Literal") && str name := key.\value) { 
            return <LitOrId::lit(string(name)), buildExpression(val), "">; 
        }
        if ((key.\type=="NumericLiteral" || key.\type=="Literal") && num v := key.\value) {
            return <LitOrId::lit(number(v)), buildExpression(val), "">; 
        }
        println("<key.\type>"); 
    }       
    
    // TODO no return?     
}

//member(Expression object, str strProperty)
//  member(Expression object, Expression expProperty)
Expression buildExpression(node _expression) {
    // if (_expression.\type=="BinaryExpression") println(_expression);
    switch(_expression.\type)  {
        case "BinaryExpression": 
            if (node left:=_expression.left 
             && node right:=_expression.right 
             && str operator:=_expression.operator)
              return binaryExpression(buildExpression(left), buildBinaryOperator(operator), buildExpression(right));
        case "LogicalExpression": 
            if (node left:=_expression.left 
             && node right:=_expression.right 
             && str operator:=_expression.operator)
              return logical(buildExpression(left), buildLogicalOperator(operator), buildExpression(right));
        case "ConditionalExpression": 
            if (node consequent:=_expression.consequent 
             && node alternate:=_expression.alternate 
             && node condition:=_expression.\test)
              return conditional(buildExpression(condition), buildExpression(consequent), buildExpression(alternate), \loc=L(_expression));
        case "UnaryExpression":
             if (bool prefix:=_expression.prefix && node argument:= _expression.argument 
                  && str operator:=_expression.operator)
                  return unary(buildUnaryOperator(operator), buildExpression(argument), prefix, \loc=L(_expression));
         case "UpdateExpression":
             if (bool prefix:=_expression.prefix && node argument:= _expression.argument 
                  && str operator:=_expression.operator)
                  return update(buildUpdateOperator(operator), buildExpression(argument), prefix, \loc=L(_expression));
        case "NumericLiteral": {
            if (num v := _expression.\value)
               return literal(number(v), \loc=L(_expression)); 
            }
         case "BooleanLiteral": {
            if (bool v := _expression.\value)
               return literal(boolean(v), \loc=L(_expression)); 
            }
        case "Literal": {
            if ((!_expression.\value?)) return literal(null()); 
            if (bool v := _expression.\value)
               return literal(boolean(v), \loc=L(_expression)); 
            if (num v := _expression.\value)
               return literal(number(v), \loc=L(_expression));
            if (str v := _expression.\value)
               return literal(string(v), \loc=L(_expression));  
            }
        case "NullLiteral": return literal(null()); 
        case "StringLiteral": {
            if (str v := _expression.\value)
               return literal(string(v), \loc=L(_expression)); 
            }
        case "RegExpLiteral": {
            if (str v := _expression.\pattern)
               return literal(regExp(v), \loc=L(_expression)); 
            }
        case "ThisExpression": return this();
        case "Identifier": {
            if (str name:=_expression.name)
                 return Expression::variable(name, \loc=L(_expression));               
            } 
        case "AssignmentExpression": {
            if (node left := _expression.left &&  
                str operator := _expression.operator && node right:=_expression.right) {
                return assignment(buildExpression(left), buildAssignmentOperator(operator),  buildExpression(right), \loc=L(_expression));
                }
            }
        case "CallExpression":
             if (list[node] arguments:= _expression.arguments 
                  && node callee:=_expression.callee)
                  return call(buildExpression(callee), [buildExpression(argument)|node argument<-arguments], \loc=L(_expression)); 
        // new(Expression callee, list[Expression] arguments)
         case "NewExpression":
             if (list[node] arguments:= _expression.arguments 
                  && node callee:=_expression.callee)
                  return new(buildExpression(callee), [buildExpression(argument)|node argument<-arguments], \loc=L(_expression)); 
        case "SequenceExpression":
             // sequence(list[Expression] expressions)
             if (list[node] expressions:= _expression.expressions)
                  return sequence([buildExpression(expression)|node expression<-expressions], \loc=L(_expression));
        case "ArrayExpression":
             if (list[node] elements:= _expression.elements)
                  return Expression::array([buildExpression(element)|node element<-elements], \loc=L(_expression));
        case "ObjectExpression":
             if (list[node] properties:= _expression.properties)
                  return Expression::object([buildObjectProperty(property)|node property<-properties], \loc=L(_expression));   
        case "FunctionExpression":
         if (list[node] params := _expression.params && node body :=  _expression.body
             && bool generator := _expression.generator && list[node] stats:=body.body) {
             if (_expression.id? && node id := _expression.id && id.name? && str name := id.name)
               return Expression::function(name, [buildIdentifier(v)|node v<-params], [buildStatement(stat)|stat<-stats], generator, \loc=L(_expression));
             return Expression::function("", [buildIdentifier(v)|node v<-params], [buildStatement(stat)|stat<-stats], generator, \loc=L(_expression));
              } 
         case "MemberExpression": {
            if (node object:=_expression.object 
              && value property:=_expression.property 
             && bool computed:=_expression.computed) {
             if (computed && node prop:=property) {
                return member(buildExpression(object), buildExpression(prop));
                }
              if (str name:=property) return member(buildExpression(object), name, \loc=L(_expression)); 
              if (node prop:=property)
                   if (str name:=prop.name) return member(buildExpression(object), name, \loc=L(_expression)); 
              } 
             }           
        } 
    println(_expression.\type);                       
    return literal(number(-1));
}
    
BinaryOperator buildBinaryOperator(str operator) {
   switch(operator) {
      case "==": return equals(); 
      case "!=":return notEquals();  
   case "===":return longEquals(); 
   case "!==":return longNotEquals();
   case "\<":return lt() ;
   case "\<=":return leq(); 
   case "\>":return gt(); 
   case "\>=":return geq();
   case "\<\<":return shiftLeft(); 
   case "\>\>":return shiftRight(); 
   case "\>\>\>":return longShiftRight();
   case "+":return BinaryOperator::plus(); 
   case "-":return BinaryOperator::min(); 
   case "*":return times();
   case "/": return div(); 
   case "%":return rem();
   case "|":return bitOr(); 
   case "^":return bitXor(); 
   case "&":return bitAnd() ;
   case ",":return comma() ;
   case "in":return \in();
   case "instanceof":return instanceOf();
    }
    println(operator);
}
  
  
AssignmentOperator buildAssignmentOperator(str operator) {
   switch(operator) {
      case "=": return assign();
      case "+=": return plusAssign();
      case "-=": return minAssign();
      case "*=": return  timesAssign() ;
      case  "/=": return divAssign();
      case  "%=": return remAssign();
      case "\<\<=": return shiftLeftAssign() ;
      case  "\>\>=": return shiftRightAssign() ;
      case  "\>\>\>=": return longShiftRightAssign();
      case  "|=": return bitOrAssign();
      case  "^=": return bitXorAssign();
      case  "&=": return  bitAndAssign(); 
   }
}

/* "-" | "+" | "!" | "~" | "typeof" | "void" | "delete" */

/* min() | plus() | not() | bitNot() | typeOf() | \void() | delete();*/

UnaryOperator buildUnaryOperator(str operator) {
    switch(operator) {
       case "-": return UnaryOperator::min();
       case "+": return UnaryOperator::plus();
       case "!": return UnaryOperator::not();
       case "~": return UnaryOperator::bitNot();
       case "typeof": return UnaryOperator::typeOf();
       case "void": return UnaryOperator::\void();
       case "delete": return UnaryOperator::delete();
    }
}
    
UpdateOperator buildUpdateOperator(str operator) {
    switch(operator) {
      case "++": return inc();
      case "--": return dec();
    }
}

LogicalOperator buildLogicalOperator(str operator) {
    switch(operator) {
      case "&&": return LogicalOperator::and();
      case "||": return LogicalOperator::or();
    }
}

@javaClass{org.rascalmpl.library.lang.javascript.m3.JavascriptParser}
private java str _parse(loc file);

@javaClass{org.rascalmpl.library.lang.javascript.m3.JavascriptParser}
private java str _parse(loc src, str code);
