export function getTaskTemplate(): string {
  return `=== ПОСТАНОВКА ЗАДАЧИ ===

Цель работы:
Изучение принципов формализации языков с помощью контекстно-свободных грамматик,
а также освоение методов синтаксического анализа на основе рекурсивного спуска
с детектированием и диагностикой ошибок.

Задание:
Разработать лексический и синтаксический анализатор для подмножества логических
выражений языка Fortran.

Язык предназначен для описания логических выражений и включает:
- логические константы;
- логические операции;
- операции сравнения;
- идентификаторы и числовые литералы;
- скобочные выражения;
- переносы строк как разделители выражений.

Алфавит языка (основные лексемы):
- Логические константы: .TRUE., .FALSE.
- Логические операции: .AND., .OR.
- Операции сравнения: .GT., .LT.
- Скобки: (, )
- Идентификаторы: последовательности букв, цифр и символа '_', начинающиеся с буквы или '_'
- Числа: последовательности цифр
- Конец строки (NL), конец файла (EOF)

Приоритет операций:
1. Операции сравнения (.GT., .LT.)
2. Логические операции (.AND., .OR.)
Скобки изменяют стандартный порядок вычисления.

Парсер должен:
- выполнять синтаксический анализ входной цепочки;
- обнаруживать и корректно диагностировать синтаксические и лексические ошибки;
- продолжать разбор после ошибок для выявления нескольких ошибок за один проход.

Примеры корректных цепочек:
1) .TRUE..AND..FALSE.
2) (X.GT.0).OR.(Y.LT.5)
3) A.LT.B.AND.C.GT.10

Примеры цепочек с ошибками:
1) .TRUE.AND.FASE.
   - Ошибка выполнения: Необходимо писать .AND., а не AND
   - Ошибка выполнения: Неизвестный операнд .FASE.

2) X.GT..AND..Y.LT.5
   - Ошибка выполнения: Между .GT. и .AND. нет второго операнда
   - Ошибка выполнения: Лишняя точка перед переменной

3) (.TRUE..OR.).LT.5
   - Ошибка выполнения: Ожидался логический операнд
   - Ошибка выполнения: Оператор сравнения .LT. не может применяться к логическому значению .TRUE.

Результатом работы анализатора является отчёт,
содержащий список обнаруженных ошибок с указанием их типа и позиции во входной цепочке.`
}

export function getBibliographyTemplate(): string {
  return `=== СПИСОК ЛИТЕРАТУРЫ ===

1. Учебное пособие "Теория языков программирования: Проектирование и реализация" Шорников Ю. В. [Электронный ресурс pdf].

2. Сравнение рекурсивного спуска и LR с примерами ошибок [Электронный ресурс]. — URL: https://news.ycombinator.com/item?id=34410776

3. Рекурсивный парсер с panic-mode для восстановления ошибок [Электронный ресурс]. — URL: https://github.com/GabrielBeloDev/recursive_parser_error_handling

4. Контекстно-свободная грамматика для логических выражений с приоритетами [Электронный ресурс]. — URL: https://stackoverflow.com/questions/65161608/implementation-of-a-context-free-grammar-for-logical-operators-with-parentheses

5. Логические константы и операторы (.TRUE., .FALSE., .AND., .OR.) [Электронный ресурс]. — URL: https://masuday.github.io/fortran_tutorial/logicalexpression.html 

6. ANTLR Documentation [Электронный ресурс]. — URL: https://www.antlr.org/

7. Rust Programming Language [Электронный ресурс]. — URL: https://www.rust-lang.org/

8. Tauri — Build smaller, faster, and more secure desktop applications [Электронный ресурс]. — URL: https://tauri.app/

9. Vue.js — The Progressive JavaScript Framework [Электронный ресурс]. — URL: https://vuejs.org/

10. TypeScript — JavaScript With Syntax For Types [Электронный ресурс]. — URL: https://www.typescriptlang.org/`
}

export function getSourceCodeHeaderTemplate(): string {
  return `=== ИСХОДНЫЙ КОД ПРОГРАММЫ ===

Автор: Шарапов Владимир Александрович
Дата создания: 15.12.2025
Версия: 1.0
Ссылка на исходный код: https://github.com/Q1zin/compiler.git`
}

export function getGrammarChoiceTemplate(): string {
  return `=== ВЫБОР ГРАММАТИКИ ===

Грамматика задаётся как G[Z] = {VT, VN, Z, P}.

Стартовый символ: Z = <program>

Терминалы (VT): VT = {
  TRUE_CONST, FALSE_CONST, AND_OP, OR_OP, GT_OP, LT_OP,
  LPAREN, RPAREN, IDENTIFIER, NUMBER, NL, EOF
}

Нетерминалы (VN): {
  <program>, <logicalExpression>, <logicalTerm>, <logicalOperand>,
  <comparison>, <comparisonOp>, <operand>
}

Продукции (P)
  1.  <program> → NL* (<logicalExpression> (NL+ <logicalExpression>)*)? NL* EOF
  2.  <logicalExpression> → <logicalTerm> (OR_OP <logicalTerm>)*
  3.  <logicalTerm> → <logicalOperand> (AND_OP <logicalOperand>)*
  4.  <logicalOperand> → TRUE_CONST | FALSE_CONST | <comparison> | LPAREN <logicalExpression> RPAREN
  5.  <comparison> → <operand> <comparisonOp> <operand>
  6.  <comparisonOp> → GT_OP | LT_OP
  7.  <operand> → <identifier> | <number>
  8.  <identifier> → IDENTIFIER
  9.  <number> → NUMBER

Семантика Терминалов
  1.  TRUE_CONST  = ".TRUE."
  2.  FALSE_CONST = ".FALSE."
  3.  AND_OP      = ".AND."
  4.  OR_OP       = ".OR."
  5.  GT_OP       = ".GT."
  6.  LT_OP       = ".LT."
  7.  LPAREN      = "("
  8.  RPAREN      = ")"
  9.  IDENTIFIER  = [A-Za-z_][A-Za-z0-9_]*
  10. NUMBER      = [0-9]+
  11. NL          = "\\r"? "\\n"+
  12. WS          = [ \\t]+                        (пропускается)

LL(1)-совместимая грамматика:
   1.  Детерминированный выбор правил по первому символу
   2.  Отсутствие левой рекурсии (используется EBNF с операторами * и ?)
   3.  Однозначный разбор с одним токеном упреждения`
}

export function getChomskyClassificationTemplate(): string {
  return `=== КЛАССИФИКАЦИЯ ХОМСКОГО ===

Обоснование классификации (Тип 2 - КС-грамматика):

Форма правил: A → α
  - Левая часть (A): ровно один нетерминал
  - Правая часть (α): произвольная цепочка из терминалов и нетерминалов

Тип 2: Контекстно-свободная грамматика (КС-грамматика)
  1.  Левая часть правил: один нетерминальный символ
  2.  Правая часть: любая последовательность терминалов и нетерминалов
  3.  Отсутствие ограничений на контекст применения правил
`
}

export function getMethod1Template(): string {
  return `=== 1 - МЕТОД (Рекурсивный спуск) ===

Метод синтаксического анализа: Нисходящий разбор методом рекурсивного спуска

Принцип работы:
  - Для каждого нетерминала создаётся отдельная функция
  - Функции вызывают друг друга рекурсивно согласно правилам грамматики
  - Разбор начинается со стартового символа <program>

Функции разбора (по нетерминалам):
- parse_program()        : <program>
- parse_lv()             : <logicalExpression>
- parse_lt()             : <logicalTerm>
- parse_lo()             : <logicalOperand>
- parse_sv()             : <comparison>
- parse_operand()        : <operand>

Выполненные условия LL(1)-совместимости:
   - Отсутствие левой рекурсии
   - Детерминированный выбор альтернатив
   - Однозначность по первому токену`
}

export function getAntlrTemplate(): string {
  return `=== ANTLR ===

ANTLR (ANother Tool for Language Recognition) - генератор парсеров

Целевой язык: TypeScript (для web-приложения)

Файл грамматики: FortranLogical.g4:
grammar FortranLogical;

// Правила парсера (нетерминалы)
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

// Правила лексера (терминалы)
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

NL : '\\r'? '\\n'+ ;
WS : [ \\t]+ -> skip ;`
}
