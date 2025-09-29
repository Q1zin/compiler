import { ref, reactive, computed } from 'vue';
import type { FileTab, EditorSettings, ProgramOutput } from '../types';
import { open as openDialog, save as saveDialog } from '@tauri-apps/plugin-dialog';
import { readTextFile, writeTextFile } from '@tauri-apps/plugin-fs';
import { openPath, revealItemInDir } from '@tauri-apps/plugin-opener';


export function useEditor() {
  // Состояние табов
  const tabs = ref<FileTab[]>([]);
  const activeTabId = ref<string | null>(null);
  let tabCounter = 0;

  // Настройки редактора
  const settings = reactive<EditorSettings>({
    fontSize: 14,
    codeFontSize: 14,
    outputFontSize: 14,
    theme: 'dark',
    tabSize: 2,
    wordWrap: false,
    showLineNumbers: true,
  });

  // Состояние вывода
  const output = ref<ProgramOutput[]>([]);
  const isRunning = ref(false);
  // Какая вкладка OutputPanel активна
  const outputActiveTab = ref<'output' | 'errors'>('output');

  // Вычисляемые свойства
  const activeTab = computed(() => 
    tabs.value.find(tab => tab.id === activeTabId.value) || null
  );

  const activeTabContent = computed(() => 
    activeTab.value?.content || ''
  );

  const hasUnsavedChanges = computed(() => 
    tabs.value.some(tab => tab.isModified)
  );

  // Генерация уникального ID для таба
  const generateTabId = (): string => {
    return `tab-${++tabCounter}-${Date.now()}`;
  };

  // Таймеры для авто-сброса подтверждения закрытия
  const confirmTimers = new Map<string, number>();

  // Создание нового таба
  const createNewTab = (name?: string, content?: string, path?: string): FileTab => {
    const tabId = generateTabId();
    const newTab: FileTab = {
      id: tabId,
      name: name || `Untitled-${tabs.value.length + 1}`,
      path: path || '',
      content: content || '// Новый файл\nconsole.log("Hello, World!");',
      // Новые (без пути) считаются изменёнными по умолчанию
      isModified: !path,
      isActive: false,
      created: new Date(),
      confirmClosePending: false,
    };

    tabs.value.push(newTab);
    setActiveTab(tabId);

    return newTab;
  };

  // Установка активного таба
  const setActiveTab = (tabId: string) => {
    tabs.value.forEach(tab => {
      tab.isActive = tab.id === tabId;
    });
    activeTabId.value = tabId;
  };

  // Закрытие таба (двойное подтверждение для несохранённых)
  const closeTab = (tabId: string) => {
    const tabIndex = tabs.value.findIndex(tab => tab.id === tabId);
    if (tabIndex === -1) return;

    const tab = tabs.value[tabIndex];

    // Проверка несохранённых изменений
    if (tab.isModified) {
      if (!tab.confirmClosePending) {
        tab.confirmClosePending = true;
        // Авто-сброс через 5 секунд
        if (confirmTimers.has(tab.id)) {
          const t = confirmTimers.get(tab.id)!;
          window.clearTimeout(t);
        }
        const timeoutId = window.setTimeout(() => {
          const target = tabs.value.find(t => t.id === tab.id);
          if (target) target.confirmClosePending = false;
          confirmTimers.delete(tab.id);
          // Убираем предупреждение, если таб не закрыт за 5 секунд
          output.value = output.value.filter((o) => !(o.type === 'warning' && o.meta?.kind === 'unsaved-close' && o.meta?.tabId === tab.id));
        }, 5000);
        confirmTimers.set(tab.id, timeoutId);
        // Удалим старые предупреждения такого типа для этой вкладки, чтобы не дублировать
        output.value = output.value.filter((o) => !(o.type === 'warning' && o.meta?.kind === 'unsaved-close' && o.meta?.tabId === tab.id));
        addOutput({
          type: 'warning',
          message: `Предупреждение: есть несохранённые изменения в файле "${tab.name}". Нажмите ещё раз, чтобы закрыть.`,
          timestamp: new Date(),
          meta: { kind: 'unsaved-close', tabId: tab.id },
        });
        return;
      }
      // Второй клик: закроем без сохранения
    }

    // Удаляем таб
    tabs.value.splice(tabIndex, 1);
    // Очистим таймер подтверждения, если был
    if (confirmTimers.has(tabId)) {
      const t = confirmTimers.get(tabId)!;
      window.clearTimeout(t);
      confirmTimers.delete(tabId);
    }
    // Немедленно убираем предупреждение про несохранённые изменения для этого таба
    output.value = output.value.filter((o) => !(o.type === 'warning' && o.meta?.kind === 'unsaved-close' && o.meta?.tabId === tabId));

    // Если закрыли активный таб, переключаемся на другой
    if (tabId === activeTabId.value) {
      if (tabs.value.length > 0) {
        const newActiveIndex = Math.min(tabIndex, tabs.value.length - 1);
        setActiveTab(tabs.value[newActiveIndex].id);
      } else {
        activeTabId.value = null;
      }
    }
  };

  // Закрытие всех табов
  const closeAllTabs = () => {
    if (hasUnsavedChanges.value) {
      console.log('Предупреждение: есть несохранённые изменения');
    }

    tabs.value = [];
    activeTabId.value = null;
  };

  // Закрытие других табов
  const closeOtherTabs = (keepTabId: string) => {
    const keepTab = tabs.value.find(tab => tab.id === keepTabId);
    if (!keepTab) return;

    const hasUnsavedInOthers = tabs.value.some(tab => tab.id !== keepTabId && tab.isModified);

    if (hasUnsavedInOthers) {
      console.log('Предупреждение: есть несохранённые изменения в других файлах');
    }

    tabs.value = [keepTab];
    setActiveTab(keepTabId);
  };

  // Обновление содержимого активного таба
  const updateActiveTabContent = (content: string) => {
    const tab = activeTab.value;
    if (!tab) return;

    tab.content = content;
    tab.isModified = true;
    // Меняем контент — сбрасываем ожидание подтверждения
    if (tab.confirmClosePending) {
      tab.confirmClosePending = false;
      if (confirmTimers.has(tab.id)) {
        const t = confirmTimers.get(tab.id)!;
        window.clearTimeout(t);
        confirmTimers.delete(tab.id);
      }
    }
  };

  // Файловые операции (демо-открытие по пути)
  const openFile = async (filePath?: string) => {
    try {
      console.log('Открытие файла:', filePath);

      const fileName = filePath ? filePath.split('/').pop() || 'example.js' : 'example.js';
      const fileContent = '// Загруженный файл\nconsole.log("Файл успешно загружен");';

      const existingTab = tabs.value.find(tab => tab.path === filePath);
      if (existingTab) {
        setActiveTab(existingTab.id);
        return existingTab;
      }

      const newTab = createNewTab(fileName, fileContent, filePath);

      addOutput({
        type: 'output',
        message: `Файл "${fileName}" успешно открыт`,
        timestamp: new Date(),
      });

      return newTab;
    } catch (error) {
      console.error('Ошибка открытия файла:', error);
      addOutput({
        type: 'error',
        message: 'Не удалось открыть файл',
        timestamp: new Date(),
      });
    }
  };

  // Реальное сохранение текущей вкладки
  const saveActiveTab = async () => {
    const tab = activeTab.value;
    if (!tab) return;

    if (!tab.path) {
      return saveActiveTabAs();
    }

    try {
      await writeTextFile(tab.path, tab.content);
      tab.isModified = false;
      // Сбрасываем подтверждение после сохранения
      if (tab.confirmClosePending) {
        tab.confirmClosePending = false;
        if (confirmTimers.has(tab.id)) {
          const t = confirmTimers.get(tab.id)!;
          window.clearTimeout(t);
          confirmTimers.delete(tab.id);
        }
      }

      addOutput({
        type: 'output',
        message: `Файл "${tab.name}" успешно сохранён`,
        timestamp: new Date(),
      });
    } catch (error) {
      console.error('Ошибка сохранения файла:', error);
      addOutput({
        type: 'error',
        message: 'Не удалось сохранить файл',
        timestamp: new Date(),
      });
    }
  };

  // Сохранить как
  const saveActiveTabAs = async () => {
    const tab = activeTab.value;
    if (!tab) return;

    try {
      const suggestedName = tab.name || 'Untitled.js';
      const newPath = await saveDialog({
        title: 'Сохранить файл как',
        defaultPath: suggestedName,
      });
      if (!newPath) return; // отмена пользователем

      await writeTextFile(newPath, tab.content);
      tab.path = newPath;
      tab.name = newPath.split('/').pop() || suggestedName;
      tab.isModified = false;

      if (tab.confirmClosePending) {
        tab.confirmClosePending = false;
        if (confirmTimers.has(tab.id)) {
          const t = confirmTimers.get(tab.id)!;
          window.clearTimeout(t);
          confirmTimers.delete(tab.id);
        }
      }

      addOutput({
        type: 'output',
        message: `Файл сохранён как "${tab.name}"`,
        timestamp: new Date(),
      });
    } catch (error) {
      console.error('Ошибка сохранения файла:', error);
      addOutput({
        type: 'error',
        message: 'Не удалось сохранить файл',
        timestamp: new Date(),
      });
    }
  };

  // Выполнение кода (демо)
  const runCode = async () => {
    const tab = activeTab.value;
    if (!tab || isRunning.value) return;

    isRunning.value = true;
    clearOutput();

    addOutput({
      type: 'output',
      message: `Запуск программы "${tab.name}"...`,
      timestamp: new Date(),
    });

    try {
      setTimeout(() => {
        try {
          let capturedOutput = '';
          const mockConsole = {
            log: (...args: any[]) => {
              capturedOutput += args.join(' ') + '\n';
            },
          };

          const code = tab.content.replace(/console\.log/g, 'mockConsole.log');
          const func = new Function('mockConsole', code);
          func(mockConsole);

          if (capturedOutput) {
            addOutput({
              type: 'output',
              message: capturedOutput.trim(),
              timestamp: new Date(),
            });
          }
        } catch (execError) {
          addOutput({
            type: 'error',
            message: `Ошибка выполнения: ${execError}`,
            timestamp: new Date(),
            file: tab.path || tab.name,
          });
        }

        // если нет ошибок после выполнения — покажем success в Errors вкладке
        const hasProblem = output.value.some((o) => o.type === 'error' || o.type === 'warning');
        if (!hasProblem) {
          addOutput({
            type: 'success',
            message: 'Компиляция и выполнение завершены успешно. Ошибок не найдено.',
            timestamp: new Date(),
          });
        } else {
          addOutput({
            type: 'output',
            message: 'Программа завершена',
            timestamp: new Date()
          });
        }
        
        isRunning.value = false;
      }, 500);
      
    } catch (error) {
      addOutput({
        type: 'error',
        message: `Ошибка выполнения: ${error}`,
        timestamp: new Date()
      });
      isRunning.value = false;
    }
  };

  // Управление выводом
  const addOutput = (outputItem: ProgramOutput) => {
    output.value.push(outputItem);
    // Переключаем вкладку: ошибки/предупреждения -> errors, success -> оставаться на "Вывод"
    if (outputItem.type === 'error' || outputItem.type === 'warning') {
      outputActiveTab.value = 'errors';
    } else if (outputItem.type === 'success') {
      // Явно оставим/переключим на вывод
      outputActiveTab.value = 'output';
    } else if (outputItem.type === 'output') {
      // Не переключаем на вывод, если уже показываем вкладку ошибок
      if (outputActiveTab.value !== 'errors') {
        outputActiveTab.value = 'output';
      }
    }
  };

  const clearOutput = () => {
    output.value = [];
  };

  // Очистить только сообщения типа 'output' (оставить ошибки/предупреждения)
  const clearOutputOnly = () => {
    output.value = output.value.filter((item) => item.type !== 'output');
  };

  // Управление настройками
  const changeCodeFontSize = (delta: number) => {
    settings.codeFontSize = Math.max(10, Math.min(24, (settings.codeFontSize ?? settings.fontSize) + delta));
  };

  const changeOutputFontSize = (delta: number) => {
    settings.outputFontSize = Math.max(10, Math.min(24, (settings.outputFontSize ?? settings.fontSize) + delta));
  };

  const toggleWordWrap = () => {
    settings.wordWrap = !settings.wordWrap;
  };

  const changeTabSize = (size: number) => {
    settings.tabSize = Math.max(2, Math.min(8, size));
  };

  // Открыть файл с диска (реально)
  async function openFileFromDisk() {
    const selected = await openDialog({
      multiple: false,
      directory: false,
    })
    if (!selected || Array.isArray(selected)) return

    const path = selected
    const content = await readTextFile(path)
    // Если вкладка пустая — заполним её, иначе создадим новую
    if (activeTab.value && !activeTab.value.path && !activeTab.value.content) {
      activeTab.value.path = path
      activeTab.value.name = path.split('/').pop() || path // фикс: имя, а не path
      updateActiveTabContent(content)
      activeTab.value.isModified = false // только что открыли — не изменён
    } else {
      const title = path.split('/').pop() || path
      // Передаём path, чтобы вкладка считалась сохранённой (не изменённой)
      createNewTab(title, content, path)
    }
  }

  async function openActiveFileExternally() {
    const path = activeTab.value?.path
    if (path) await openPath(path)
  }

  async function revealActiveFileInFolder() {
    const path = activeTab.value?.path
    if (path) await revealItemInDir(path)
  }

  // Текстовые операции
  const insertTaskTemplate = () => {
    const template = `/*
ПОСТАНОВКА ЗАДАЧИ

Цель: Описать цель программы

Входные данные:
- Параметр 1: описание
- Параметр 2: описание

Выходные данные:
- Результат: описание

Алгоритм:
1. Шаг 1
2. Шаг 2
3. Шаг 3
*/

`;
    const tab = activeTab.value;
    if (tab) {
      tab.content = template + tab.content;
      tab.isModified = true;
    }
  };

  const insertBibliography = () => {
    const bibliography = `
/*
СПИСОК ЛИТЕРАТУРЫ

1. Автор А.А. Название книги. - М.: Издательство, 2023. - 300 с.
2. Автор Б.Б. Статья в журнале // Название журнала. - 2023. - №1. - С. 15-25.
3. Интернет-ресурс: https://example.com - Название ресурса
*/`;
    
    const tab = activeTab.value;
    if (tab) {
      tab.content = tab.content + '\n\n' + bibliography;
      tab.isModified = true;
    }
  };

  const addSourceCodeComment = () => {
    const comment = `/*
=== ИСХОДНЫЙ КОД ПРОГРАММЫ ===
Автор: 
Дата создания: ${new Date().toLocaleDateString()}
Версия: 1.0
*/

`;
    const tab = activeTab.value;
    if (tab) {
      tab.content = comment + tab.content;
      tab.isModified = true;
    }
  };

  // Инициализация
  const initialize = () => {};

  // Инициализируем при создании
  initialize();

  return {
    // Состояние табов
    tabs,
    activeTabId,
    activeTab,
    activeTabContent,
    hasUnsavedChanges,
    
    // Настройки
    settings,
    output,
    isRunning,
  outputActiveTab,
    
    // Операции с табами
    createNewTab,
    setActiveTab,
    closeTab,
    closeAllTabs,
    closeOtherTabs,
    updateActiveTabContent,
    
    // Файловые операции
    openFile,
    saveActiveTab,
    saveActiveTabAs,
    
    // Выполнение кода
    runCode,
    
    // Управление выводом
    addOutput,
  clearOutput,
  clearOutputOnly,
    
    // Настройки
  changeCodeFontSize,
  changeOutputFontSize,
    toggleWordWrap,
    changeTabSize,
    
    // Текстовые операции
    insertTaskTemplate,
    insertBibliography,
    addSourceCodeComment,
    
    // Инициализация
    initialize,

    // Действия с реальными файлами
    openFileFromDisk,
    openActiveFileExternally,
    revealActiveFileInFolder,
  };
}