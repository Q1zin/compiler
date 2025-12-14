<script setup lang="ts">
import { ref, computed, onMounted, onBeforeUnmount, watch } from 'vue'
import MenuBar from './components/MenuBar.vue'
import TabManager from './components/TabManager.vue'
import CodeEditor from './components/CodeEditor.vue'
import OutputPanel from './components/OutputPanel.vue'
import TextModal from './components/TextModal.vue'
import { useEditor } from './composables/useEditor.ts'
import { getTaskTemplate, getBibliographyTemplate, getSourceCodeHeaderTemplate } from './templates/textTemplates'

const {
  tabs,
  activeTabId,
  activeTab,
  activeTabContent,
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
  changeCodeFontSize,
  changeOutputFontSize,
  insertTaskTemplate,
  insertBibliography,
  addSourceCodeComment,
  openFileFromDisk,
  openActiveFileExternally,
  revealActiveFileInFolder,
  clearErrors,
} = useEditor()

const handleFileAction = async (action: string) => {
  
  switch (action) {
    case 'new':
      createNewTab()
      break
    case 'open':
      await openFileFromDisk()
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
  
  switch (action) {
    case 'task':
      modalTitle.value = 'Постановка задачи'
      modalText.value = getTaskTemplate()
      modalKind.value = 'task'
      showTextModal.value = true
      break
    case 'bibliography':
      modalTitle.value = 'Список литературы'
      modalText.value = getBibliographyTemplate()
      modalKind.value = 'bibliography'
      showTextModal.value = true
      break
    case 'source-code':
      modalTitle.value = 'Исходный код программы'
      modalText.value = getSourceCodeHeaderTemplate()
      modalKind.value = 'source-code'
      showTextModal.value = true
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

const handleTabSelect = (tabId: string) => {
  setActiveTab(tabId)
}

const handleTabClose = (tabId: string) => {
  closeTab(tabId)
}

const handleTabNew = () => {
  createNewTab()
}

const handleCodeUpdate = (newCode: string) => {
  updateActiveTabContent(newCode)
}

const editorCode = computed({
  get: () => activeTabContent.value,
  set: (value: string) => handleCodeUpdate(value)
})

const activeFileName = computed(() => 
  activeTab.value?.name || 'Нет открытых файлов'
)

const outputItems = computed(() => output.value)

const handleClearErrors = () => {
  clearErrors()
}

const showTextModal = ref(false)
const modalTitle = ref('')
const modalText = ref('')
const modalKind = ref<'task' | 'bibliography' | 'source-code' | null>(null)
const closeTextModal = () => { showTextModal.value = false }
const insertTextFromModal = () => {
  if (!activeTab.value) {
    createNewTab(undefined, '')
  }
  if (modalKind.value === 'task') insertTaskTemplate()
  else if (modalKind.value === 'bibliography') insertBibliography()
  else if (modalKind.value === 'source-code') addSourceCodeComment()
  showTextModal.value = false
}

const handleToggleTheme = () => {
  settings.theme = settings.theme === 'dark' ? 'light' : 'dark'
}

const editorContainerEl = ref<HTMLElement | null>(null)
const outputWidthPx = ref<number | null>(null)
let isResizing = false
let resizeStartX = 0
let resizeStartWidth = 0

const clamp = (value: number, min: number, max: number) => Math.max(min, Math.min(max, value))

const ensureDefaultOutputWidth = () => {
  if (!editorContainerEl.value) return
  if (outputWidthPx.value !== null) return
  const w = editorContainerEl.value.getBoundingClientRect().width
  outputWidthPx.value = Math.round(w * 0.7)
}

const startResize = (e: MouseEvent) => {
  if (!editorContainerEl.value) return
  ensureDefaultOutputWidth()
  if (outputWidthPx.value === null) return

  isResizing = true
  resizeStartX = e.clientX
  resizeStartWidth = outputWidthPx.value

  document.body.style.cursor = 'col-resize'
  document.body.style.userSelect = 'none'

  e.preventDefault()
}

const onResizeMove = (e: MouseEvent) => {
  if (!isResizing) return
  if (!editorContainerEl.value) return
  if (outputWidthPx.value === null) return

  const containerWidth = editorContainerEl.value.getBoundingClientRect().width
  const minWidth = Math.max(240, Math.round(containerWidth * 0.25))
  const maxWidth = Math.max(minWidth, Math.round(containerWidth * 0.8))

  const dx = e.clientX - resizeStartX
  const nextWidth = resizeStartWidth - dx
  outputWidthPx.value = clamp(Math.round(nextWidth), minWidth, maxWidth)
}

const stopResize = () => {
  if (!isResizing) return
  isResizing = false
  document.body.style.cursor = ''
  document.body.style.userSelect = ''
}

onMounted(() => {
  window.addEventListener('mousemove', onResizeMove)
  window.addEventListener('mouseup', stopResize)
  window.addEventListener('blur', stopResize)
  if (activeTab.value) ensureDefaultOutputWidth()
})

onBeforeUnmount(() => {
  window.removeEventListener('mousemove', onResizeMove)
  window.removeEventListener('mouseup', stopResize)
  window.removeEventListener('blur', stopResize)
})

watch(
  () => activeTab.value,
  tab => {
    if (tab) {
      ensureDefaultOutputWidth()
    }
  },
  { immediate: true }
)
</script>

<template>
  <div :class="['app', `theme-${settings.theme}`]">
    <MenuBar 
      @file-action="handleFileAction"
      @text-action="handleTextAction"
      @run-code="handleRunCode"
      @font-size-change="handleFontSizeChange"
      @toggle-theme="handleToggleTheme"
    />
    
    <TabManager 
      :tabs="tabs"
      :active-tab-id="activeTabId"
      @tab-select="handleTabSelect"
      @tab-close="handleTabClose"
      @tab-new="handleTabNew"
    />
    
    <div ref="editorContainerEl" class="editor-container">
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
      
      <div
        v-if="activeTab"
        class="output-wrapper"
        :style="outputWidthPx !== null ? { width: `${outputWidthPx}px` } : undefined"
      >
        <div class="resize-handle" @mousedown="startResize" />
        <OutputPanel
          :items="outputItems"
          :font-size="settings.outputFontSize ?? settings.fontSize"
          class="output-section"
          @clear-output="handleClearErrors()"
        />
      </div>
  
      <TextModal
        :show="showTextModal"
        :title="modalTitle"
        :text="modalText"
        @close="closeTextModal"
        @insert="insertTextFromModal"
      />
    </div>
  </div>
</template>

<style scoped>
.app {
  height: 100vh;
  display: flex;
  flex-direction: column;
  background: var(--bg-primary);
  color: var(--text-primary);
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
}

.editor-container {
  flex: 1;
  display: flex;
  overflow: hidden;
}

.code-section {
  flex: 1;
  border-right: 1px solid var(--border-color);
  display: flex;
  flex-direction: column;
}

.output-wrapper {
  flex: 0 0 auto;
  position: relative;
  display: flex;
  height: 100%;
}

.resize-handle {
  position: absolute;
  left: 0;
  top: 0;
  bottom: 0;
  width: 6px;
  cursor: col-resize;
}

.editor-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  background: var(--bg-secondary);
  padding: 8px 16px;
  border-bottom: 1px solid var(--border-color);
  font-size: 12px;
  min-height: 32px;
}

.file-name {
  color: var(--text-primary);
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
  background: var(--bg-primary);
}

.no-file-message {
  text-align: center;
  padding: 40px;
}

.no-file-message h3 {
  color: var(--text-primary);
  margin-bottom: 12px;
  font-size: 20px;
  font-weight: 600;
}

.no-file-message p {
  color: var(--text-muted);
  margin-bottom: 24px;
  font-size: 14px;
}

.quick-actions {
  display: flex;
  gap: 12px;
  justify-content: center;
}

.action-button {
  background: var(--border-color);
  border: none;
  color: var(--text-primary);
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
  background: var(--accent-hover);
}

.action-button.primary {
  background: var(--accent);
  color: white;
}

.action-button.primary:hover {
  background: var(--accent-hover);
}

.output-section {
  flex: 1;
  width: 100%;
}

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

</style>