grammar FortranLogical;

program
    : NL* (logicalExpression (NL+ logicalExpression)*)? NL* EOF
    ;

logicalExpression
    : logicalTerm (OR_OP logicalTerm)*
    ;

logicalTerm
    : logicalOperand (AND_OP logicalOperand)*
    ;

logicalOperand
    : TRUE_CONST
    | FALSE_CONST
    | comparison
    | LPAREN logicalExpression RPAREN
    ;

comparison
    : operand comparisonOp operand
    ;

comparisonOp
    : GT_OP
    | LT_OP
    ;

operand
    : identifier
    | number
    ;

identifier
    : IDENTIFIER
    ;

number
    : NUMBER
    ;

TRUE_CONST  : '.TRUE.' ;
FALSE_CONST : '.FALSE.' ;

AND_OP : '.AND.' ;
OR_OP  : '.OR.' ;

GT_OP : '.GT.' ;
LT_OP : '.LT.' ;

LPAREN : '(' ;
RPAREN : ')' ;

IDENTIFIER
    : LETTER (LETTER | DIGIT)*
    ;

NUMBER
    : DIGIT+
    ;

fragment LETTER : [A-Za-z] ;
fragment DIGIT  : [0-9] ;

NL : '\r'? '\n'+ ;
WS : [ \t]+ -> skip ;
