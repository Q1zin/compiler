import { ref, reactive, computed } from 'vue';
import type { FileTab, EditorSettings, ProgramOutput } from '../types';
import { open as openDialog, save as saveDialog } from '@tauri-apps/plugin-dialog';
import { readTextFile, writeTextFile } from '@tauri-apps/plugin-fs';
import { openPath, revealItemInDir } from '@tauri-apps/plugin-opener';
import { invoke } from '@tauri-apps/api/core';
import { getTaskTemplate, getBibliographyTemplate, getSourceCodeHeaderTemplate } from '../templates/textTemplates';


export function useEditor() {
  const tabs = ref<FileTab[]>([]);
  const activeTabId = ref<string | null>(null);
  let tabCounter = 0;

  const settings = reactive<EditorSettings>({
    fontSize: 14,
    codeFontSize: 14,
    outputFontSize: 14,
    theme: 'dark',
    tabSize: 2,
    wordWrap: false,
    showLineNumbers: true,
  });

  const output = ref<ProgramOutput[]>([]);
  const isRunning = ref(false);
  const outputActiveTab = ref<'output' | 'errors'>('output');

  const activeTab = computed(() => 
    tabs.value.find(tab => tab.id === activeTabId.value) || null
  );

  const activeTabContent = computed(() => 
    activeTab.value?.content || ''
  );

  const hasUnsavedChanges = computed(() => 
    tabs.value.some(tab => tab.isModified)
  );

  const generateTabId = (): string => {
    return `tab-${++tabCounter}-${Date.now()}`;
  };

  const confirmTimers = new Map<string, number>();

  const createNewTab = (name?: string, content?: string, path?: string): FileTab => {
    const tabId = generateTabId();
    const newTab: FileTab = {
      id: tabId,
      name: name || `Untitled-${tabs.value.length + 1}`,
      path: path || '',
      content: content ?? '// Новый файл\nconsole.log("Hello, World!");',
      isModified: !path,
      isActive: false,
      created: new Date(),
      confirmClosePending: false,
    };

    tabs.value.push(newTab);
    setActiveTab(tabId);

    return newTab;
  };

  const setActiveTab = (tabId: string) => {
    tabs.value.forEach(tab => {
      tab.isActive = tab.id === tabId;
    });
    activeTabId.value = tabId;
  };

  const closeTab = (tabId: string) => {
    const tabIndex = tabs.value.findIndex(tab => tab.id === tabId);
    if (tabIndex === -1) return;

    const tab = tabs.value[tabIndex];

    if (tab.isModified) {
      if (!tab.confirmClosePending) {
        tab.confirmClosePending = true;
        if (confirmTimers.has(tab.id)) {
          const t = confirmTimers.get(tab.id)!;
          window.clearTimeout(t);
        }
        const timeoutId = window.setTimeout(() => {
          const target = tabs.value.find(t => t.id === tab.id);
          if (target) target.confirmClosePending = false;
          confirmTimers.delete(tab.id);
          output.value = output.value.filter((o) => !(o.type === 'warning' && o.meta?.kind === 'unsaved-close' && o.meta?.tabId === tab.id));
        }, 5000);
        confirmTimers.set(tab.id, timeoutId);
        output.value = output.value.filter((o) => !(o.type === 'warning' && o.meta?.kind === 'unsaved-close' && o.meta?.tabId === tab.id));
        addOutput({
          type: 'warning',
          message: `Предупреждение: есть несохранённые изменения в файле "${tab.name}". Нажмите ещё раз, чтобы закрыть.`,
          timestamp: new Date(),
          meta: { kind: 'unsaved-close', tabId: tab.id },
        });
        return;
      }
    }

    tabs.value.splice(tabIndex, 1);
    if (confirmTimers.has(tabId)) {
      const t = confirmTimers.get(tabId)!;
      window.clearTimeout(t);
      confirmTimers.delete(tabId);
    }
    output.value = output.value.filter((o) => !(o.type === 'warning' && o.meta?.kind === 'unsaved-close' && o.meta?.tabId === tabId));

    if (tabId === activeTabId.value) {
      if (tabs.value.length > 0) {
        const newActiveIndex = Math.min(tabIndex, tabs.value.length - 1);
        setActiveTab(tabs.value[newActiveIndex].id);
      } else {
        activeTabId.value = null;
      }
    }
  };

  const closeAllTabs = () => {
    if (hasUnsavedChanges.value) {
      console.log('Предупреждение: есть несохранённые изменения');
    }

    tabs.value = [];
    activeTabId.value = null;
  };

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

  const updateActiveTabContent = (content: string) => {
    const tab = activeTab.value;
    if (!tab) return;

    tab.content = content;
    tab.isModified = true;
    if (tab.confirmClosePending) {
      tab.confirmClosePending = false;
      if (confirmTimers.has(tab.id)) {
        const t = confirmTimers.get(tab.id)!;
        window.clearTimeout(t);
        confirmTimers.delete(tab.id);
      }
    }
  };

  const saveActiveTab = async () => {
    const tab = activeTab.value;
    if (!tab) return;

    if (!tab.path) {
      return saveActiveTabAs();
    }

    try {
      await writeTextFile(tab.path, tab.content);
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

  const saveActiveTabAs = async () => {
    const tab = activeTab.value;
    if (!tab) return;

    try {
      const suggestedName = tab.name || 'Untitled.js';
      const newPath = await saveDialog({
        title: 'Сохранить файл как',
        defaultPath: suggestedName,
      });
      if (!newPath) return;

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

    // .gg files: validate syntax via Rust parser (no evaluation)
    if (tab.name.toLowerCase().endsWith('.gg')) {
      await validateGgFile(tab);
      return;
    }

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
          let line: number | undefined
          let column: number | undefined
          const stack = (execError as any)?.stack as string | undefined
          if (stack) {
            const re = /:(\d+):(\d+)/
            const m = re.exec(stack)
            if (m) {
              line = Number(m[1])
              column = Number(m[2])
            }
          }
          addOutput({
            type: 'error',
            message: `Ошибка выполнения: ${execError}`,
            timestamp: new Date(),
            file: tab.path || tab.name,
            line,
            column,
          });
        }

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
      let line: number | undefined
      let column: number | undefined
      const stack = (error as any)?.stack as string | undefined
      if (stack) {
        const re = /:(\d+):(\d+)/
        const m = re.exec(stack)
        if (m) {
          line = Number(m[1])
          column = Number(m[2])
        }
      }
      addOutput({
        type: 'error',
        message: `Ошибка выполнения: ${error}`,
        timestamp: new Date(),
        line,
        column,
      });
      isRunning.value = false;
    }
  };

  const validateGgFile = async (tab: FileTab) => {
    try {
      const res = await invoke<{ ok: boolean; messages: { kind: string; message: string; line: number; column: number }[] }>(
        'validate_expression',
        { input: tab.content }
      );

      const file = tab.path || tab.name;
      if (res.messages.length) {
        for (const m of res.messages) {
          addOutput({
            type: 'error',
            message: m.message,
            timestamp: new Date(),
            file,
            line: m.line,
            column: m.column,
          });
        }
        addOutput({
          type: 'output',
          message: 'Проверка завершена: найдены ошибки',
          timestamp: new Date(),
        });
      } else {
        addOutput({
          type: 'success',
          message: 'Цепочка корректна. Ошибок не найдено.',
          timestamp: new Date(),
        });
      }
    } catch (e) {
      addOutput({
        type: 'error',
        message: `Ошибка выполнения: ${e}`,
        timestamp: new Date(),
        file: tab.path || tab.name,
      });
    } finally {
      isRunning.value = false;
    }
  };

  const addOutput = (outputItem: ProgramOutput) => {
    output.value.push(outputItem);
    if (outputItem.type === 'error' || outputItem.type === 'warning') {
      outputActiveTab.value = 'errors';
    } else if (outputItem.type === 'success') {
      outputActiveTab.value = 'output';
    } else if (outputItem.type === 'output') {
      if (outputActiveTab.value !== 'errors') {
        outputActiveTab.value = 'output';
      }
    }
  };

  const clearOutput = () => {
    output.value = [];
  };

  const clearOutputOnly = () => {
    output.value = output.value.filter((item) => item.type !== 'output');
  };

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

  const toggleTheme = () => {
    settings.theme = settings.theme === 'dark' ? 'light' : 'dark'
  }

  async function openFileFromDisk() {
    const selected = await openDialog({
      multiple: false,
      directory: false,
    })
    if (!selected || Array.isArray(selected)) return

    const path = selected
    const content = await readTextFile(path)
    if (activeTab.value && !activeTab.value.path && !activeTab.value.content) {
      activeTab.value.path = path
      activeTab.value.name = path.split('/').pop() || path
      updateActiveTabContent(content)
      activeTab.value.isModified = false
    } else {
      const title = path.split('/').pop() || path
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

  const insertTaskTemplate = () => {
    const template = `/*\n${getTaskTemplate()}\n*/\n\n`;
    const tab = activeTab.value;
    if (tab) {
      tab.content = template + tab.content;
      tab.isModified = true;
    }
  };

  const insertBibliography = () => {
    const bibliography = `/*\n${getBibliographyTemplate()}\n*/\n\n`;
    const tab = activeTab.value;
    if (tab) {
      tab.content = tab.content + '\n\n' + bibliography;
      tab.isModified = true;
    }
  };

  const addSourceCodeComment = () => {
    const comment = `/*\n${getSourceCodeHeaderTemplate()}\n*/\n\n`;
    const tab = activeTab.value;
    if (tab) {
      tab.content = comment + tab.content;
      tab.isModified = true;
    }
  };

  const initialize = () => {};

  initialize();

  return {
    tabs,
    activeTabId,
    activeTab,
    activeTabContent,
    hasUnsavedChanges,
    settings,
    output,
    isRunning,
    outputActiveTab,
    createNewTab,
    setActiveTab,
    closeTab,
    closeAllTabs,
    closeOtherTabs,
    updateActiveTabContent,
    saveActiveTab,
    saveActiveTabAs,
    runCode,
    addOutput,
    clearOutput,
    clearOutputOnly,
    changeCodeFontSize,
    changeOutputFontSize,
    toggleWordWrap,
    changeTabSize,
  toggleTheme,
    insertTaskTemplate,
    insertBibliography,
    addSourceCodeComment,
    initialize,
    openFileFromDisk,
    openActiveFileExternally,
    revealActiveFileInFolder,
  };
}