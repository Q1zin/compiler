export function getTaskTemplate(): string {
  return `ПОСТАНОВКА ЗАДАЧИ

Цель: Описать цель программы

Входные данные:
- Параметр 1: описание
- Параметр 2: описание

Выходные данные:
- Результат: описание

Алгоритм:
1. Шаг 1
2. Шаг 2
3. Шаг 3`
}

export function getBibliographyTemplate(): string {
  return `СПИСОК ЛИТЕРАТУРЫ
...`
}

export function getSourceCodeHeaderTemplate(): string {
  return `=== ИСХОДНЫЙ КОД ПРОГРАММЫ ===
Автор: Шарапов Владимир Александрович
Дата создания: ...
Версия: 1.0
Ссылка на исходный код: https://github.com/Q1zin/compiler.git`
}
