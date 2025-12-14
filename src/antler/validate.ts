import { CharStreams, CommonTokenStream } from 'antlr4ts';
import type { ANTLRErrorListener } from 'antlr4ts/ANTLRErrorListener';
import type { RecognitionException } from 'antlr4ts/RecognitionException';
import type { Token } from 'antlr4ts/Token';
import { FortranLogicalLexer } from './generated/FortranLogicalLexer';
import { FortranLogicalParser } from './generated/FortranLogicalParser';

export interface AntlerValidationMessage {
  message: string;
  line: number;
  column: number;
}

export function validateWithAntler(input: string): AntlerValidationMessage[] {
  const messages: AntlerValidationMessage[] = [];

  const listener: ANTLRErrorListener<Token> = {
    syntaxError: (
      _recognizer,
      _offendingSymbol,
      line,
      charPositionInLine,
      msg,
      _e: RecognitionException | undefined
    ) => {
      messages.push({
        message: msg,
        line,
        column: Math.max(1, charPositionInLine + 1),
      });
    },
  };

  const chars = CharStreams.fromString(input);
  const lexer = new FortranLogicalLexer(chars);
  lexer.removeErrorListeners();
  lexer.addErrorListener(listener);

  const tokens = new CommonTokenStream(lexer);
  const parser = new FortranLogicalParser(tokens);
  parser.removeErrorListeners();
  parser.addErrorListener(listener);

  parser.buildParseTree = false;
  parser.program();

  return messages;
}
