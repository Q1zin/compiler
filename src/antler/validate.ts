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

  const normalized = input.replace(/\r\n/g, '\n').replace(/\r/g, '\n');
  const lines = normalized.split('\n');

  for (let idx = 0; idx < lines.length; idx += 1) {
    const lineText = lines[idx];
    if (lineText.trim().length === 0) continue;

    const lineOffset = idx;
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
          line: lineOffset + Math.max(1, line),
          column: Math.max(1, charPositionInLine + 1),
        });
      },
    };

    const chars = CharStreams.fromString(lineText);
    const lexer = new FortranLogicalLexer(chars);
    lexer.removeErrorListeners();
    lexer.addErrorListener(listener);

    const tokens = new CommonTokenStream(lexer);
    const parser = new FortranLogicalParser(tokens);
    parser.removeErrorListeners();
    parser.addErrorListener(listener);

    parser.buildParseTree = false;
    parser.program();
  }

  return messages;
}
