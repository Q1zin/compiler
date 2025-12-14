import fs from 'node:fs/promises';
import path from 'node:path';

const generatedDir = path.resolve('src/antler/generated');

const removableImportLines = [
  'import { NotNull } from "antlr4ts/Decorators";',
  'import { Override } from "antlr4ts/Decorators";',
  'import { RuleContext } from "antlr4ts/RuleContext";',
  'import { Token } from "antlr4ts/Token";',
];

const parserOnlyRemovableImportLines = [
  'import { ParseTreeListener } from "antlr4ts/tree/ParseTreeListener";',
  'import { ParseTreeVisitor } from "antlr4ts/tree/ParseTreeVisitor";',
];

const tokenImportLine = 'import { Token } from "antlr4ts/Token";';

function patch(content, fileName) {
  let out = content;
  for (const line of removableImportLines) {
    out = out.replace(new RegExp(`^${line.replace(/[.*+?^${}()|[\\]\\]/g, '\\$&')}\\r?\\n`, 'm'), '');
  }

  if (fileName.endsWith('Parser.ts')) {
    for (const line of parserOnlyRemovableImportLines) {
      out = out.replace(new RegExp(`^${line.replace(/[.*+?^${}()|[\\]\\]/g, '\\$&')}\\r?\\n`, 'm'), '');
    }
  }

  if (!out.includes(tokenImportLine) && /\bToken\s*\./.test(out)) {
    if (/^import .*\r?\n\r?\n/m.test(out)) {
      out = out.replace(/^import .*\r?\n\r?\n/m, m => `${m}${tokenImportLine}\n`);
    } else {
      out = `${tokenImportLine}\n${out}`;
    }
  }

  out = out.replace(/^\s*\/\/.*\r?\n/gm, '');
  out = out.replace(/\/\*[\s\S]*?\*\//g, '');

  out = out.replace(/\n{3,}/g, '\n\n');
  return out;
}

const entries = await fs.readdir(generatedDir, { withFileTypes: true });
const tsFiles = entries.filter(e => e.isFile() && e.name.endsWith('.ts')).map(e => e.name);

let changedCount = 0;
for (const name of tsFiles) {
  const filePath = path.join(generatedDir, name);
  const before = await fs.readFile(filePath, 'utf8');
  const after = patch(before, name);
  if (after !== before) {
    await fs.writeFile(filePath, after, 'utf8');
    changedCount += 1;
  }
}
