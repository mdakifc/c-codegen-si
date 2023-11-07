program : Include macros

macros : macros 
       | '#define' Identifier Constant
       | globals
       ;

globals : globals
        | globalVariableDefinition
        | functionDefinition
        ;

// TODO: type-constant consolidation
globalVariableDefinition 
    : 'const'? type Identifier ('[' Int? ']')? '=' Constant; 
    | 'const'? type '*'? Identifier '=' Constant; 
    ;

functionDefinition
    : functionDefinition 
    | type identifier parameterList '{' body '}'
    | main 
    ;

parameterList 
    : '(' (parameter (',' parameter)*)? ')'
    ;

parameter:
    : 'const'? type ('[' Int? ']')? Identifier
    | 'const'? type '*'? Identifier
    ;

main : 'int main(int argc, char * argv[])' {' body '}'

body : statement*;

statement 
    : variableDeclaration ';'
    | variableDefinition ';'
    | assignment ';'
    | loop
    | ifconditions
    ;

vectorizableStatement 
    : assignment ';'
    | loop ';'
    ;

loop : forLoopForward
     | forLoopBackward


// Config on compare

forLoopForward : 'for' '(' INT Identifier '=' expression ';' Identifier ('<' '<=', '!=') Constant ';' increment ')' '{' vectorizableBody '}'
forLoopBackward : 'for' '(' INT Identifier '=' expression ';' Identifier ('>', '>=') Constant ';' decrement ')' '{' vectorizableBody '}'

increment : Identifier '+=' Int
decrement : Identifier '-=' Int

vectorizableBody : vectorizableStatement* ;

variableDeclaration 
    : 'const'? type Identifier ('[' Int? ']')?
    | 'const'? type '*'? Identifier
    ;

variableDefinition 
    : variableDeclaration = expression;
    ; 

assignment : lvalue = expression ;
lvalue : *? identifier | identifier ('[' expression ']')? ;

vectorExpression : // TODO
expression 
    : Constant
    | identifier
    | identifier ('[' expression ']')? 
    | expression + expression
    | expression - expression
    | expression * expression
    | expression / expression
    | // Compare, bit ops, vector ops
    ;

Include : '#include <global.h>' ;
Constant : Int | Float | Char | String | IntitializerList;
