grammar FortranLogical;

program
    : logicalExpression EOF
    ;

logicalExpression
    : logicalTerm (OR_OP logicalTerm)*
    ;

logicalTerm
    : logicalOperand (AND_OP logicalOperand)*
    ;

logicalOperand
    : TRUE_CONST                                    # TrueConst
    | FALSE_CONST                                   # FalseConst
    | comparison                                    # ComparisonExpr
    | LPAREN logicalExpression RPAREN               # ParenExpr
    ;

comparison
    : operand comparisonOp operand
    ;

comparisonOp
    : GT_OP     # GreaterThan
    | LT_OP     # LessThan
    ;

operand
    : identifier    # IdentifierOperand
    | number        # NumberOperand
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

WS : [ \t\r\n]+ -> skip ;
