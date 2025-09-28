import { ref, reactive } from 'vue';
import type { FileInfo, EditorSettings, ProgramOutput } from '../types';

export function useEditor() {
  // Состояние файла
  const currentFile = ref<FileInfo | null>(null);
  const code = ref('// Добро пожаловать в редактор кода\nconsole.log("Hello, World!");');
  const isModified = ref(false);

  // Настройки редактора
  const settings = reactive<EditorSettings>({
    fontSize: 14,
    theme: 'dark',
    tabSize: 2,
    wordWrap: false,
    showLineNumbers: true
  });

  // Состояние вывода
  const output = ref<ProgramOutput[]>([]);
  const isRunning = ref(false);

  // Файловые операции
  const createNewFile = () => {
    if (isModified.value) {
      // Показать диалог сохранения
      console.log('Предупреждение: есть несохранённые изменения');
    }
    
    currentFile.value = {
      name: 'Untitled.js',
      path: '',
      content: '',
      modified: false
    };
    code.value = '';
    isModified.value = false;
  };

  const openFile = async () => {
    try {
      // Здесь будет интеграция с Tauri API
      console.log('Открытие файла...');
      
      // Симуляция открытия файла
      const mockFile: FileInfo = {
        name: 'example.js',
        path: '/path/to/example.js',
        content: '// Загруженный файл\nconsole.log("Файл успешно загружен");',
        modified: false
      };
      
      currentFile.value = mockFile;
      code.value = mockFile.content;
      isModified.value = false;
    } catch (error) {
      console.error('Ошибка открытия файла:', error);
      addOutput({
        type: 'error',
        message: 'Не удалось открыть файл',
        timestamp: new Date()
      });
    }
  };

  const saveFile = async () => {
    if (!currentFile.value) {
      return saveFileAs();
    }

    try {
      // Здесь будет интеграция с Tauri API
      console.log('Сохранение файла:', currentFile.value.path);
      
      currentFile.value.content = code.value;
      currentFile.value.modified = false;
      isModified.value = false;
      
      addOutput({
        type: 'output',
        message: `Файл "${currentFile.value.name}" успешно сохранён`,
        timestamp: new Date()
      });
    } catch (error) {
      console.error('Ошибка сохранения файла:', error);
      addOutput({
        type: 'error',
        message: 'Не удалось сохранить файл',
        timestamp: new Date()
      });
    }
  };

  const saveFileAs = async () => {
    try {
      // Здесь будет интеграция с Tauri API для диалога сохранения
      console.log('Сохранение файла как...');
      
      const newFile: FileInfo = {
        name: 'new-file.js',
        path: '/path/to/new-file.js',
        content: code.value,
        modified: false
      };
      
      currentFile.value = newFile;
      isModified.value = false;
      
      addOutput({
        type: 'output',
        message: `Файл сохранён как "${newFile.name}"`,
        timestamp: new Date()
      });
    } catch (error) {
      console.error('Ошибка сохранения файла:', error);
    }
  };

  const closeFile = () => {
    if (isModified.value) {
      // Показать диалог сохранения
      console.log('Предупреждение: есть несохранённые изменения');
    }
    
    currentFile.value = null;
    code.value = '';
    isModified.value = false;
  };

  // Операции с кодом
  const updateCode = (newCode: string) => {
    code.value = newCode;
    isModified.value = true;
    if (currentFile.value) {
      currentFile.value.modified = true;
    }
  };

  const runCode = async () => {
    if (isRunning.value) return;
    
    isRunning.value = true;
    clearOutput();
    
    addOutput({
      type: 'output',
      message: 'Запуск программы...',
      timestamp: new Date()
    });

    try {
      // Здесь будет интеграция с системой выполнения кода
      // Симуляция выполнения
      setTimeout(() => {
        addOutput({
          type: 'output',
          message: 'Hello, World!',
          timestamp: new Date()
        });
        
        addOutput({
          type: 'output',
          message: 'Программа завершена успешно',
          timestamp: new Date()
        });
        
        isRunning.value = false;
      }, 1000);
      
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
  };

  const clearOutput = () => {
    output.value = [];
  };

  // Управление настройками
  const changeFontSize = (delta: number) => {
    settings.fontSize = Math.max(10, Math.min(24, settings.fontSize + delta));
  };

  const toggleWordWrap = () => {
    settings.wordWrap = !settings.wordWrap;
  };

  const changeTabSize = (size: number) => {
    settings.tabSize = Math.max(2, Math.min(8, size));
  };

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
    code.value = template + code.value;
    isModified.value = true;
  };

  const insertBibliography = () => {
    const bibliography = `/*
СПИСОК ЛИТЕРАТУРЫ

1. Автор А.А. Название книги. - М.: Издательство, 2023. - 300 с.
2. Автор Б.Б. Статья в журнале // Название журнала. - 2023. - №1. - С. 15-25.
3. Интернет-ресурс: https://example.com - Название ресурса
*/

`;
    code.value = code.value + '\n\n' + bibliography;
    isModified.value = true;
  };

  const addSourceCodeComment = () => {
    const comment = `
/*
=== ИСХОДНЫЙ КОД ПРОГРАММЫ ===
Автор: 
Дата создания: ${new Date().toLocaleDateString()}
Версия: 1.0
*/

`;
    code.value = comment + code.value;
    isModified.value = true;
  };

  return {
    // Состояние
    currentFile,
    code,
    isModified,
    settings,
    output,
    isRunning,
    
    // Файловые операции
    createNewFile,
    openFile,
    saveFile,
    saveFileAs,
    closeFile,
    
    // Операции с кодом
    updateCode,
    runCode,
    
    // Управление выводом
    addOutput,
    clearOutput,
    
    // Настройки
    changeFontSize,
    toggleWordWrap,
    changeTabSize,
    
    // Текстовые операции
    insertTaskTemplate,
    insertBibliography,
    addSourceCodeComment
  };
}