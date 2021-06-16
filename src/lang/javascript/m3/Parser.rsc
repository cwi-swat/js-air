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
import Type;
import lang::json::IO;
import util::ShellExec;
import ValueIO;

list[int] line2Offset = [];

Declaration parseJavascript(loc js) {
   code = readFile(js);
   line2Offset = [0] + [i | i <- findAll(code, "\n")];
   str src = _parse(js, code);
   node json = parseJSON(#node, src, implicitConstructors=true, implicitNodes=true);
   return buildProgram(json);
}

loc L(node n) {
    if (n.\loc? && node location:= n.\loc && str src :=location.source && node \start := location.\start
          && node end:=location.end && int line1:=\start.line && int column1:=\start.column
          && int line2:=\end.line && int column2:=\end.column
       )  {
         column1 += 1;
         column2 += 1;
         //println("col1 <column1> col2 <column2>");
         offset = line2Offset[line1-1] + column1;
         endOffset = line2Offset[line2-1] + column2;
         len = endOffset - offset;
         //println("line1 <line1> offsetl1 <line2Offset[line1]>, line2 <line2> offsetl2 <line2Offset[line2]>, offset <offset>, len <len>");
       return str2loc(src)(offset, len, <line1, column1>, <line2, column2>);
    }
    
    return |file:///|; 
}

private loc str2loc(str s)
  = readTextValueString(#loc, "|<s>|");

private &T cast(type[&T] t, value x) {
  if (&T v := x) {
    return v;
  } else {
    throw "expected <t> but got <Type::typeOf(x)>";
  }
}

private node \node(value x) = cast(#node, x);
private list[node] \nodes(list[value] x) = cast(#list[node], x);

Declaration build(node file) = buildProgram(cast(#node, file.program));

Declaration buildProgram(node _program) 
  = program([buildStatement(statement) | statement <- cast(#list[node], _program.body)], src=L(_program));
  
Statement buildStatement(node _statement) {
    switch(_statement.\type)  {
     case "VariableDeclaration": 
      if (list[node] variableDeclarators:=_statement.declarations) {
         if (str kind:= ((_statement.kind?)?_statement.kind:"var"))
        return 
         Statement::varDecl([<buildIdentifier(id), buildInit(variableDeclarator)> |node variableDeclarator<-variableDeclarators, node id := variableDeclarator.id]
            , kind, src=L(_statement));
            }
     case "FunctionDeclaration":
         if (node id := _statement.id && list[node] params := _statement.params && node body :=  _statement.body
             && bool generator := _statement.generator && list[node] stats:=body.body && str name := id.name) {
               return functionDecl(name, [buildIdentifier(v)|node v<-params], [buildStatement(stat)|stat<-stats], generator, src=L(_statement));
               }
     case "ExpressionStatement": 
        if (node _expression:=_statement.expression)  return Statement::expression(buildExpression(_expression), src=L(_statement));
     case "EmptyStatement": return Statement::empty();
     case "BlockStatement": {
      if (_statement.body?)
      if (list[node] stats:=_statement.body)
        return 
         Statement::block([buildStatement(stat)|stat<-stats], src=L(_statement)); 
     // labeled(str label, Statement stat)
        if (_statement.block? && node block:=_statement.block) 
            return Statement::block(buildStatement(block)); 
        }
     case "LabeledStatement":
         if (value label:=_statement.label && node body:= _statement.body) {
               if  (node lbl:=label && str name := lbl.name) return labeled(name, buildStatement(body));
               if  (str name:=label) return labeled(name, buildStatement(body), src=L(_statement));
               }
     case "IfStatement":  
        if (node condition:=_statement.\test && node consequent:=_statement.consequent)
          if (_statement.alternate?) {
               if (node alternate :=_statement.alternate)
                  return Statement::\if(buildExpression(condition), buildStatement(consequent), buildStatement(alternate), src=L(_statement));
            }
          else  
            return Statement::\if(buildExpression(condition), buildStatement(consequent)); 
            // \switch(Expression discriminant, list[SwitchCase] cases)
     case "SwitchStatement": 
          if (node discriminant:= _statement.discriminant && list[node] cases:= _statement.cases)
                return \switch(buildExpression(discriminant), [buildSwitchCase(\case)|node \case<-cases], src=L(_statement));
     case "ForStatement": {
         forInit = empty(); 
        if (_statement.init? && node init := _statement.init) {
              if (init.\type=="VariableDeclaration" && list[node] variableDeclarators:= init.declarations 
                     && str kind:= init.kind) {
              forInit =  
                varDecl([<buildIdentifier(i), buildInit(variableDeclarator)>|node variableDeclarator<-variableDeclarators, node i := variableDeclarator.id], kind, src=L(_statement));
              }
              else forInit = expression(buildExpression(init))[src=L(_statement)];
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
           return \for(forInit, condition, buildStatement(body), update, src=L(_statement));
           }  
        }
     case "ForInStatement": {
        if (node _right := _statement.right && node body := _statement.body)
        if (node _left := _statement.left) {
              if (_left.\type=="VariableDeclaration" && list[node] variableDeclarators:= _left.declarations 
                     && str kind:= _left.kind) {
                return forIn(varDecl([<buildIdentifier(i), buildInit(variableDeclarator)>|node variableDeclarator<-variableDeclarators, node i := variableDeclarator.id]) 
                            , kind 
                            , buildExpression(_right)
                            , buildStatement(body));
              }
            return forIn(buildExpression(_left),buildExpression(_right),buildStatement(body), src=L(_statement));
            } 
        }
     case "WhileStatement": {
        if( node condition := _statement.\test && node body:=_statement.body)
           return \while(buildExpression(condition), buildStatement(body), src=L(_statement));
        }
     case "DoWhileStatement": {
        if( node condition := _statement.\test && node body:=_statement.body)
           return doWhile(buildStatement(body), buildExpression(condition), src=L(_statement));
        }  
     case "ReturnStatement": {
        if (_statement.argument? && node argument := _statement.argument)
          return \return(buildExpression(argument), src=L(_statement));
        return(\return());
        }
     case "BreakStatement": {
        if (_statement.label? && node label := _statement.label && str name:=label.name)
          return \break(name);
        return(\break());
        }
      case "ContinueStatement": {
        if (_statement.label? && node label := _statement.label && str name:=label.name)
          return \continue(name, src=L(_statement));
        return(\continue());
        }
       case "ThrowStatement": {
        if (node argument := _statement.argument)
          return \throw(buildExpression(argument), src=L(_statement));
        }
        case "TryStatement": {
            if (node block:= _statement.block && list[node] stats := block.body) 
                if (_statement.handler? && node handler := _statement.handler) {
                    if (_statement.finalizer? && node finalizer:=_statement.finalizer 
                       && list[node] stats1:=finalizer.body)
                      return \try([buildStatement(stat)|stat<-stats], buildCatchClause(handler), 
                       [buildStatement(stat)|stat<-stats1] );
                 return \try([buildStatement(stat)|stat<-stats], buildCatchClause(handler), src=L(_statement));
                 } else 
                   if (_statement.finalizer? && node finalizer:=_statement.finalizer 
                       && list[node] stats1:=finalizer.body)
                   {
                   return \try([buildStatement(stat)|stat<-stats], [buildStatement(stat)|stat<-stats1], src=L(_statement) );
                   }            
            }
      }
}
    
    
Statement buildVariableDeclarator(node _variableDeclarator) 
  = varDecl([<buildIdentifier(\node(_variableDeclarator.id)), buildInit(_variableDeclarator)>], src=L(_variableDeclarator));
    
Expression buildIdentifier(node i) = id(cast(#str, i.name), src=L(i));
    
Expression buildInit(node _variableDeclarator) = _variableDeclarator.init? ? buildExpression(\node(_variableDeclarator.init)) : undefined(); 

Statement buildSwitchCase(node _switchCase) {
  if (list[node] stats:= _switchCase.consequent) {
     if (_switchCase.\test? && node condition:=_switchCase.\test) 
        return switchCase(buildExpression(condition), 
        [buildStatement(stat)|stat<-stats]);  
     return switchCase([buildStatement(stat)|stat<-stats], src=L(_switchCase));
     }

  throw "unexpected case <_switchCase>";      
}

Statement buildCatchClause(node _catchClause) {
    if (node param:=_catchClause.param && node _body:=_catchClause.body 
           && list[node] stats := _body.body && str name:=param.name) 
        return catchClause(variable(name), [buildStatement(stat)|stat<-stats], src=L(_catchClause));
   
    throw "unexpected catch <_catchClause>";
}
    
tuple[Expression key, Expression \value, str kind]  buildObjectProperty(node _objectProperty) {
    if (node key:=_objectProperty.key && node val := _objectProperty.\value) {
        if (key.\type=="Identifier" && str name := key.name)
            return <id(name,src=L(key)), buildExpression(val), "">; 
        if ((key.\type=="StringLiteral"|| key.\type=="Literal") && str name := key.\value) { 
            return <string(name,src=L(key)), buildExpression(val), "">; 
        }
        if ((key.\type=="NumericLiteral" || key.\type=="Literal") && num v := key.\value) {
            return <number(v,src=L(key)), buildExpression(val), "">; 
        }
        println("<key.\type>"); 
    }       
    
    throw "unexpected property <_objectProperty>";
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
              return buildBinaryOperator(buildExpression(left), operator, buildExpression(right))[src=L(_expression)];
        case "LogicalExpression": 
            if (node left:=_expression.left 
             && node right:=_expression.right 
             && str operator:=_expression.operator)
              return buildLogicalOperator(buildExpression(left), operator, buildExpression(right));
        case "ConditionalExpression": 
            if (node consequent:=_expression.consequent 
             && node alternate:=_expression.alternate 
             && node condition:=_expression.\test)
              return conditional(buildExpression(condition), buildExpression(consequent), buildExpression(alternate), src=L(_expression));
        case "UnaryExpression":
             if (bool prefix:=_expression.prefix && node argument:= _expression.argument 
                  && str operator:=_expression.operator)
                  return buildUnaryOperator(buildExpression(argument), operator)[src=L(_expression)];
         case "UpdateExpression":
             if (bool prefix:=_expression.prefix && node argument:= _expression.argument 
                  && str operator:=_expression.operator)
                  return buildUpdateOperator(operator, buildExpression(argument), prefix)[src=L(_expression)];
        case "NumericLiteral": {
            if (num v := _expression.\value)
               return number(v, src=L(_expression)); 
            }
         case "BooleanLiteral": {
            if (bool v := _expression.\value)
               return boolean(v, src=L(_expression)); 
            }
        case "Literal": {
            if ((!_expression.\value?)) return null(src=L(_expression)); 
            if (bool v := _expression.\value)
               return boolean(v, src=L(_expression)); 
            if (num v := _expression.\value)
               return number(v, src=L(_expression));
            if (str v := _expression.\value)
               return string(v, src=L(_expression));  
            }
        case "NullLiteral": return null(src=L(_expression)); 
        case "StringLiteral": {
            if (str v := _expression.\value)
               return string(v, src=L(_expression)); 
            }
        case "RegExpLiteral": {
            if (str v := _expression.\pattern)
               return regExp(v, src=L(_expression)); 
            }
        case "ThisExpression": return this();
        case "Identifier": {
            if (str name:=_expression.name)
                 return Expression::variable(name, src=L(_expression));               
            } 
        case "AssignmentExpression": {
            if (node left := _expression.left &&  
                str operator := _expression.operator && node right:=_expression.right) {
                return buildAssignmentOperator(buildExpression(left), operator,  buildExpression(right))[src=L(_expression)];
                }
            }
        case "CallExpression":
             if (list[node] arguments:= _expression.arguments 
                  && node callee:=_expression.callee)
                  return call(buildExpression(callee), [buildExpression(argument)|node argument<-arguments], src=L(_expression)); 
        // new(Expression callee, list[Expression] arguments)
         case "NewExpression":
             if (list[node] arguments:= _expression.arguments 
                  && node callee:=_expression.callee)
                  return new(buildExpression(callee), [buildExpression(argument)|node argument<-arguments], src=L(_expression)); 
        case "SequenceExpression":
             // sequence(list[Expression] expressions)
             if (list[node] expressions:= _expression.expressions)
                  return sequence([buildExpression(expression)|node expression<-expressions], src=L(_expression));
        case "ArrayExpression":
             if (list[node] elements:= _expression.elements)
                  return Expression::array([buildExpression(element)|node element<-elements], src=L(_expression));
        case "ObjectExpression":
             if (list[node] properties:= _expression.properties)
                  return Expression::object([buildObjectProperty(property)|node property<-properties], src=L(_expression));   
        case "FunctionExpression":
         if (list[node] params := _expression.params && node body :=  _expression.body
             && bool generator := _expression.generator && list[node] stats:=body.body) {
             if (_expression.id? && node id := _expression.id && id.name? && str name := id.name)
               return Expression::function(name, [buildIdentifier(v)|node v<-params], [buildStatement(stat)|stat<-stats], generator, src=L(_expression));
             return Expression::function("", [buildIdentifier(v)|node v<-params], [buildStatement(stat)|stat<-stats], generator, src=L(_expression));
              } 
         case "MemberExpression": {
            if (node object:=_expression.object 
              && value property:=_expression.property 
             && bool computed:=_expression.computed) {
             if (computed && node prop:=property) {
                return member(buildExpression(object), buildExpression(prop));
                }
              if (str name:=property) return member(buildExpression(object), name, src=L(_expression)); 
              if (node prop:=property)
                   if (str name:=prop.name) return member(buildExpression(object), name, src=L(_expression)); 
              } 
             }           
        } 
        
    throw "unexpected expression <_expression>";
    //return number(-1);
}
    
Expression buildBinaryOperator(Expression l, str operator, Expression r) {
   switch(operator) {
      case "==": return equals(l,r); 
      case "!=":return notEquals(l,r);  
      case "===":return longEquals(l,r); 
      case "!==":return longNotEquals(l,r);
      case "\<":return lt(l,r) ;
      case "\<=":return leq(l,r); 
      case "\>":return gt(l,r); 
      case "\>=":return geq(l,r);
      case "\<\<":return shiftLeft(l,r); 
      case "\>\>":return shiftRight(l,r); 
      case "\>\>\>":return longShiftRight(l,r);
      case "+":return Expression::plus(l,r); 
      case "-":return Expression::min(l,r); 
      case "*":return times(l,r);
      case "/": return div(l,r); 
      case "%":return rem(l,r);
      case "|":return bitOr(l,r); 
      case "^":return bitXor(l,r); 
      case "&":return bitAnd(l,r) ;
      case ",":return comma(l,r) ;
      case "in":return \in(l,r);
      case "instanceof":return instanceOf(l,r);
      default: 
        throw "unsupported operator <operator>";
    }
}
  
  
Expression buildAssignmentOperator(Expression lhs, str operator, Expression rhs) {
   switch(operator) {
      case "=": return assign(lhs,rhs);
      case "+=": return plusAssign(lhs,rhs);
      case "-=": return minAssign(lhs,rhs);
      case "*=": return  timesAssign(lhs,rhs) ;
      case  "/=": return divAssign(lhs,rhs);
      case  "%=": return remAssign(lhs,rhs);
      case "\<\<=": return shiftLeftAssign(lhs,rhs) ;
      case  "\>\>=": return shiftRightAssign(lhs,rhs) ;
      case  "\>\>\>=": return longShiftRightAssign(lhs,rhs);
      case  "|=": return bitOrAssign(lhs,rhs);
      case  "^=": return bitXorAssign(lhs,rhs);
      case  "&=": return  bitAndAssign(lhs,rhs); 
      default:
        throw "unexpected assignment operator <operator>";
   }
}

Expression buildUnaryOperator(Expression exp, str operator) {
    switch(operator) {
       case "-": return Expression::min(exp);
       case "+": return Expression::plus(exp);
       case "!": return Expression::not(exp);
       case "~": return Expression::bitNot(exp);
       case "typeof": return Expression::typeOf(exp);
       case "void": return Expression::\void(exp);
       case "delete": return Expression::delete(exp);
    }
    
    throw "unknown unary operator <operator>";
}
    
Expression buildUpdateOperator("++", Expression e, true) = updatePreInc(e);
Expression buildUpdateOperator("--", Expression e, true) = updatePreDec(e);
Expression buildUpdateOperator("++", Expression e, false) = updatePostInc(e);
Expression buildUpdateOperator("--", Expression e, false) = updatePostDec(e);

Expression buildLogicalOperator(Expression lhs, str operator, Expression rhs) {
    switch(operator) {
      case "&&": return and(lhs, rhs);
      case "||": return or(lhs, rhs);
    }
    
    throw "unknown logical operator <operator>";
}

@javaClass{org.rascalmpl.library.lang.javascript.m3.JavascriptParser}
private java str _parse(loc file);

@javaClass{org.rascalmpl.library.lang.javascript.m3.JavascriptParser}
private java str _parse(loc src, str code);
