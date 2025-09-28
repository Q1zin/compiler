// Типы для файловых операций
export interface FileInfo {
  name: string;
  path: string;
  content: string;
  modified: boolean;
}

// Типы для меню
export type MenuAction = 
  | 'new' 
  | 'open' 
  | 'save' 
  | 'save-as' 
  | 'close'
  | 'task'
  | 'bibliography'
  | 'source-code'
  | 'run'
  | 'font-increase'
  | 'font-decrease';

// Типы для настроек редактора
export interface EditorSettings {
  fontSize: number;
  theme: 'dark' | 'light';
  tabSize: number;
  wordWrap: boolean;
  showLineNumbers: boolean;
}

// Типы для вывода программы
export interface ProgramOutput {
  type: 'output' | 'error' | 'warning';
  message: string;
  timestamp: Date;
  line?: number;
  column?: number;
}

// Типы для вкладок панели вывода
export interface OutputTab {
  id: string;
  name: string;
  icon: string;
  count?: number;
}

// Типы для состояния приложения
export interface AppState {
  currentFile: FileInfo | null;
  recentFiles: FileInfo[];
  editorSettings: EditorSettings;
  output: ProgramOutput[];
  isRunning: boolean;
}

// События компонентов
export interface MenuEvents {
  'file-action': (action: MenuAction) => void;
  'text-action': (action: MenuAction) => void;
  'run-code': () => void;
  'font-size-change': (change: number) => void;
}

export interface EditorEvents {
  'update:modelValue': (value: string) => void;
  'cursor-change': (line: number, column: number) => void;
}

export interface OutputEvents {
  'clear-output': () => void;
  'tab-change': (tabId: string) => void;
}