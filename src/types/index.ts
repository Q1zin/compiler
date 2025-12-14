export interface FileInfo {
  name: string;
  path: string;
  content: string;
  modified: boolean;
}

export interface FileTab {
  id: string;
  name: string;
  path: string;
  content: string;
  isModified: boolean;
  isActive: boolean;
  language?: string;
  created: Date;
  confirmClosePending?: boolean;
}

export type MenuAction = 
  | 'new' 
  | 'open' 
  | 'save' 
  | 'save-as' 
  | 'close'
  | 'close-all'
  | 'close-others'
  | 'task'
  | 'bibliography'
  | 'source-code'
  | 'run'
  | 'font-increase'
  | 'font-decrease';

export interface EditorSettings {
  fontSize: number;
  codeFontSize?: number;
  outputFontSize?: number;
  theme: 'dark' | 'light';
  tabSize: number;
  wordWrap: boolean;
  showLineNumbers: boolean;
}
export interface ProgramOutput {
  type: 'output' | 'error' | 'warning' | 'success';
  message: string;
  timestamp: Date;
  line?: number;
  column?: number;
  file?: string;
  meta?: {
    kind: string;
    tabId?: string;
    parser?: 'rust' | 'antler';
  };
}
export interface OutputTab {
  id: string;
  name: string;
  icon: string;
  count?: number;
}
export interface AppState {
  tabs: FileTab[];
  activeTabId: string | null;
  recentFiles: FileInfo[];
  editorSettings: EditorSettings;
  output: ProgramOutput[];
  isRunning: boolean;
}
export interface MenuEvents {
  'file-action': (action: MenuAction) => void;
  'text-action': (action: MenuAction) => void;
  'run-code': () => void;
  'font-size-change': (area: 'code' | 'output', change: number) => void;
}

export interface EditorEvents {
  'update:modelValue': (value: string) => void;
  'cursor-change': (line: number, column: number) => void;
}

export interface OutputEvents {
  'clear-output': () => void;
  'tab-change': (tabId: string) => void;
}

export interface TabEvents {
  'tab-select': (tabId: string) => void;
  'tab-close': (tabId: string) => void;
  'tab-new': () => void;
  path?: string;
}