import { ref, reactive, computed } from 'vue';
import type { FileTab, EditorSettings, ProgramOutput } from '../types';
import { open as openDialog, save as saveDialog } from '@tauri-apps/plugin-dialog';
import { readTextFile, writeTextFile } from '@tauri-apps/plugin-fs';
import { openPath, revealItemInDir } from '@tauri-apps/plugin-opener';
import { invoke } from '@tauri-apps/api/core';
import { getTaskTemplate, getBibliographyTemplate, getSourceCodeHeaderTemplate } from '../templates/textTemplates';
import { validateWithAntler } from '../antler/validate';


export function useEditor() {
  const tabs = ref<FileTab[]>([]);
  const activeTabId = ref<string | null>(null);
  let tabCounter = 0;

  let validateTimer: number | null = null;

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
      content: content ?? 'X.GT.0.OR.Y.LT.5',
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
    tabs.value = [];
    activeTabId.value = null;
  };

  const closeOtherTabs = (keepTabId: string) => {
    const keepTab = tabs.value.find(tab => tab.id === keepTabId);
    if (!keepTab) return;

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

    scheduleValidateActiveTab();
  };

  const scheduleValidateActiveTab = () => {
    if (!activeTab.value) return;

    if (validateTimer !== null) {
      window.clearTimeout(validateTimer);
      validateTimer = null;
    }

    const tabIdAtSchedule = activeTab.value.id;
    validateTimer = window.setTimeout(async () => {
      validateTimer = null;

      const tab = tabs.value.find(t => t.id === tabIdAtSchedule);
      if (!tab) return;

      // Avoid concurrent runs; if still running, try again shortly.
      if (isRunning.value) {
        scheduleValidateActiveTab();
        return;
      }

      isRunning.value = true;
      clearOutput();
      await validateFile(tab);
    }, 200);
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
      const suggestedName = tab.name || 'Untitled.gg';
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

    await validateFile(tab);
  };

  const validateFile = async (tab: FileTab) => {
    const file = tab.path || tab.name;

    try {
      // 1) Rust/Tauri validator
      try {
        const res = await invoke<{ ok: boolean; messages: { kind: string; message: string; line: number; column: number }[] }>(
          'validate_expression',
          { input: tab.content }
        );

        for (const m of res.messages) {
          addOutput({
            type: 'error',
            message: m.message,
            timestamp: new Date(),
            file,
            line: m.line,
            column: m.column,
            meta: { kind: 'parser-error', parser: 'rust' },
          });
        }
      } catch (e) {
        addOutput({
          type: 'error',
          message: `Ошибка выполнения: ${e}`,
          timestamp: new Date(),
          file,
          meta: { kind: 'parser-crash', parser: 'rust' },
        });
      }

      // 2) Antler (antlr4ts) validator
      try {
        const antlerMessages = validateWithAntler(tab.content);
        for (const m of antlerMessages) {
          addOutput({
            type: 'error',
            message: m.message,
            timestamp: new Date(),
            file,
            line: m.line,
            column: m.column,
            meta: { kind: 'parser-error', parser: 'antler' },
          });
        }
      } catch (e) {
        addOutput({
          type: 'error',
          message: `Ошибка выполнения: ${e}`,
          timestamp: new Date(),
          file,
          meta: { kind: 'parser-crash', parser: 'antler' },
        });
      }
    } finally {
      isRunning.value = false;
    }
  };

  const addOutput = (outputItem: ProgramOutput) => {
    output.value.push(outputItem);
  };

  const clearOutput = () => {
    output.value = [];
  };

  const clearErrors = () => {
    output.value = [];
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
    clearErrors,
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