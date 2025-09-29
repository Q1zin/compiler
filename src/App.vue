<script setup lang="ts">
import { computed } from 'vue'
import MenuBar from './components/MenuBar.vue'
import TabManager from './components/TabManager.vue'
import CodeEditor from './components/CodeEditor.vue'
import OutputPanel from './components/OutputPanel.vue'
import { useEditor } from './composables/useEditor.ts'

const {
  // Состояние табов
  tabs,
  activeTabId,
  activeTab,
  activeTabContent,
  
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
  saveActiveTab,
  saveActiveTabAs,
  
  // Выполнение кода
  runCode,
  
  // Настройки
  changeCodeFontSize,
  changeOutputFontSize,
  
  // Текстовые операции
  insertTaskTemplate,
  insertBibliography,
  addSourceCodeComment,

  openFileFromDisk,
  openActiveFileExternally,
  revealActiveFileInFolder,
  // вывод
  clearOutputOnly,
} = useEditor()

// Обработчики событий меню
const handleFileAction = async (action: string) => {
  console.log('File action:', action)
  
  switch (action) {
    case 'new':
      createNewTab()
      break
    case 'open':
      await openFileFromDisk()
      // await openFile()
      break
    case 'openExternally':
      await openActiveFileExternally()
      break
    case 'revealInFolder':
      await revealActiveFileInFolder()
      break
    case 'save':
      await saveActiveTab()
      break
    case 'save-as':
      await saveActiveTabAs()
      break
    case 'close':
      if (activeTabId.value) {
        closeTab(activeTabId.value)
      }
      break
    case 'close-all':
      closeAllTabs()
      break
    case 'close-others':
      if (activeTabId.value) {
        closeOtherTabs(activeTabId.value)
      }
      break
  }
}

const handleTextAction = (action: string) => {
  console.log('Text action:', action)
  
  switch (action) {
    case 'task':
      insertTaskTemplate()
      break
    case 'bibliography':
      insertBibliography()
      break
    case 'source-code':
      addSourceCodeComment()
      break
  }
}

const handleRunCode = () => {
  runCode()
}

const handleFontSizeChange = (area: 'code' | 'output', change: number) => {
  if (area === 'code') changeCodeFontSize(change)
  else changeOutputFontSize(change)
}

// Обработчики событий табов
const handleTabSelect = (tabId: string) => {
  setActiveTab(tabId)
}

const handleTabClose = (tabId: string) => {
  closeTab(tabId)
}

const handleTabNew = () => {
  createNewTab()
}

// Обработка изменений кода
const handleCodeUpdate = (newCode: string) => {
  updateActiveTabContent(newCode)
}

// Вычисляемое свойство для передачи в редактор
const editorCode = computed({
  get: () => activeTabContent.value,
  set: (value: string) => handleCodeUpdate(value)
})

// Название активного файла для отображения
const activeFileName = computed(() => 
  activeTab.value?.name || 'Нет открытых файлов'
)

// Структурированные элементы вывода
const outputItems = computed(() => output.value)

// Синхронизация активной вкладки панели вывода
const handleOutputTabChange = (tabId: string) => {
  // установим активную вкладку в состоянии composable
  // напрямую изменяем ref
  outputActiveTab.value = tabId as 'output' | 'errors'
}
</script>

<template>
  <div class="app">
    <MenuBar 
      @file-action="handleFileAction"
      @text-action="handleTextAction"
      @run-code="handleRunCode"
      @font-size-change="handleFontSizeChange"
    />
    
    <TabManager 
      :tabs="tabs"
      :active-tab-id="activeTabId"
      @tab-select="handleTabSelect"
      @tab-close="handleTabClose"
      @tab-new="handleTabNew"
    />
    
    <div class="editor-container">
      <div class="code-section">
        <div class="editor-header">
          <span class="file-name">{{ activeFileName }}</span>
          <div class="editor-status">
            <span v-if="activeTab?.isModified" class="modified-indicator">Изменён</span>
            <span v-if="isRunning" class="running-indicator">Выполняется...</span>
          </div>
        </div>
        
        <CodeEditor 
          v-if="activeTab"
          v-model="editorCode"
          :font-size="settings.codeFontSize ?? settings.fontSize"
          class="code-editor-content"
        />
        
        <div v-else class="no-file-state">
          <div class="no-file-message">
            <h3>Добро пожаловать в редактор кода</h3>
            <p>Создайте новый файл или откройте существующий</p>
            <div class="quick-actions">
              <button @click="createNewTab()" class="action-button primary">
                📄 Новый файл
              </button>
              <button @click="openFileFromDisk()" class="action-button">
                📂 Открыть файл
              </button>
            </div>
          </div>
        </div>
      </div>
      
      <OutputPanel 
        :items="outputItems"
        :font-size="settings.outputFontSize ?? settings.fontSize"
        :active-tab="outputActiveTab"
        class="output-section"
        @clear-output="clearOutputOnly()"
        @tab-change="handleOutputTabChange"
      />
    </div>
  </div>
</template>

<style scoped>
.app {
  height: 100vh;
  display: flex;
  flex-direction: column;
  background: #1e1e1e;
  color: #d4d4d4;
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
}

.editor-container {
  flex: 1;
  display: flex;
  overflow: hidden;
}

.code-section {
  flex: 1;
  border-right: 1px solid #3e3e3e;
  display: flex;
  flex-direction: column;
}

.editor-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  background: #2d2d30;
  padding: 8px 16px;
  border-bottom: 1px solid #3e3e3e;
  font-size: 12px;
  min-height: 32px;
}

.file-name {
  color: #d4d4d4;
  font-weight: 500;
}

.editor-status {
  display: flex;
  align-items: center;
  gap: 12px;
}

.modified-indicator {
  color: #f9c23c;
  font-size: 11px;
}

.running-indicator {
  color: #4ec9b0;
  font-size: 11px;
  animation: pulse 1.5s infinite;
}

@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.5; }
}

.code-editor-content {
  flex: 1;
}

.no-file-state {
  flex: 1;
  display: flex;
  align-items: center;
  justify-content: center;
  background: #1e1e1e;
}

.no-file-message {
  text-align: center;
  padding: 40px;
}

.no-file-message h3 {
  color: #d4d4d4;
  margin-bottom: 12px;
  font-size: 20px;
  font-weight: 600;
}

.no-file-message p {
  color: #888;
  margin-bottom: 24px;
  font-size: 14px;
}

.quick-actions {
  display: flex;
  gap: 12px;
  justify-content: center;
}

.action-button {
  background: #3e3e3e;
  border: none;
  color: #d4d4d4;
  padding: 8px 16px;
  border-radius: 4px;
  cursor: pointer;
  font-size: 13px;
  transition: all 0.2s;
  display: flex;
  align-items: center;
  gap: 6px;
}

.action-button:hover {
  background: #4a4a4a;
}

.action-button.primary {
  background: #0e639c;
  color: white;
}

.action-button.primary:hover {
  background: #1177bb;
}

.output-section {
  width: 400px;
  min-width: 300px;
  max-width: 50%;
}

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

</style>