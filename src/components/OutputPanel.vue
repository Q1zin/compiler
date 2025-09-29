<script setup lang="ts">
import { ref, computed, watch } from 'vue'
import type { ProgramOutput } from '../types'

const props = defineProps<{
  items?: ProgramOutput[]
  output?: string
  errors?: string
  fontSize: number
  activeTab?: 'output' | 'errors'
}>()

const emit = defineEmits<{
  'clear-output': []
  'tab-change': [tabId: string]
}>()

const currentTab = ref<'output' | 'errors'>(props.activeTab ?? 'output')

watch(
  () => props.activeTab,
  (val) => {
    if (val && val !== currentTab.value) currentTab.value = val
  }
)

const tabs = [
  { id: 'output', name: 'Вывод', icon: '📄' },
  { id: 'errors', name: 'Ошибки', icon: '⚠️' },
]

const outputLines = computed(() => {
  // Back-compat: if items are provided, assemble output from them
  if (props.items && props.items.length) {
    const lines = props.items.filter(i => i.type === 'output').map(i => i.message)
    if (lines.length === 0) return []
    return lines.join('\n').split('\n')
  }
  if (props.output === undefined || props.output === null) {
    return ['Программа ещё не запускалась...']
  }
  if (props.output === '') {
    return []
  }
  return props.output.split('\n')
})

const problems = computed(() => (props.items ?? []).filter(i => i.type === 'error' || i.type === 'warning'))
const hasProblems = computed(() => problems.value.length > 0)
const hasSuccess = computed(() => (props.items ?? []).some(i => i.type === 'success'))

const clearOutput = () => {
  emit('clear-output')
}

const selectTab = (tabId: 'output' | 'errors') => {
  currentTab.value = tabId
  emit('tab-change', tabId)
}
</script>

<template>
  <div class="output-panel">
    <div class="panel-header">
      <div class="tabs">
        <button 
          v-for="tab in tabs" 
          :key="tab.id"
          class="tab"
          :class="{ active: currentTab === tab.id }"
          @click="selectTab(tab.id as 'output' | 'errors')"
        >
          <span class="tab-icon">{{ tab.icon }}</span>
          <span>{{ tab.name }}</span>
        </button>
      </div>
      <div style="margin-left:auto; padding-right:8px;">
        <button v-if="currentTab === 'output'" class="action-btn" @click="clearOutput">Очистить</button>
      </div>
    </div>
    
    <div class="panel-content">
      <!-- Вывод программы -->
      <div v-if="currentTab === 'output'" class="output-content">
        <div 
          v-for="(line, index) in outputLines" 
          :key="index"
          class="output-line"
          :style="{ fontSize: fontSize + 'px' }"
        >
          {{ line }}
        </div>
      </div>
      
      <!-- Ошибки / Предупреждения / Успех -->
      <div v-else-if="currentTab === 'errors'" class="error-content">
        <!-- Успешная компиляция -->
        <div v-if="!hasProblems && hasSuccess" class="success-banner">
          <span class="success-icon">✅</span>
          <div class="success-text" :style="{ fontSize: fontSize + 'px' }">
            Компиляция завершена успешно. Ошибок не найдено.
          </div>
        </div>

        <!-- Список проблем -->
        <div v-else>
          <div 
            v-for="(p, idx) in problems" 
            :key="idx"
            :class="['problem-block', p.type]"
          >
            <div class="problem-icon">{{ p.type === 'error' ? '❌' : '⚠️' }}</div>
            <div class="problem-body">
              <div class="problem-title" :style="{ fontSize: (fontSize + 1) + 'px' }">
                {{ p.message }}
              </div>
              <div v-if="p.line || p.column || p.file" class="problem-meta" :style="{ fontSize: (fontSize - 2) + 'px' }">
                <span v-if="p.file" class="meta-file">{{ p.file }}</span>
                <span v-if="p.line">строка: {{ p.line }}</span>
                <span v-if="p.column">столбец: {{ p.column }}</span>
              </div>
              <div class="problem-time" :style="{ fontSize: (fontSize - 3) + 'px' }">{{ new Date(p.timestamp).toLocaleTimeString() }}</div>
            </div>
          </div>
          <div v-if="!problems.length" class="no-errors" :style="{ fontSize: fontSize + 'px' }">
            <span class="success-icon">✅</span>
            <span>Ошибок и предупреждений нет</span>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.output-panel {
  display: flex;
  flex-direction: column;
  height: 100%;
  background: #1e1e1e;
}

.panel-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  background: #2d2d30;
  border-bottom: 1px solid #3e3e3e;
  padding: 0;
}

.tabs {
  display: flex;
}

.tab {
  background: none;
  border: none;
  color: #888;
  padding: 8px 12px;
  cursor: pointer;
  font-size: 12px;
  display: flex;
  align-items: center;
  gap: 4px;
  transition: all 0.2s;
  border-bottom: 2px solid transparent;
}

.tab:hover {
  background: #3e3e3e;
  color: #d4d4d4;
}

.tab.active {
  color: #d4d4d4;
  border-bottom-color: #0e639c;
  background: #1e1e1e;
}

.tab-icon {
  font-size: 14px;
}

.action-btn {
  background: none;
  border: none;
  color: #888;
  cursor: pointer;
  padding: 4px 8px;
  border-radius: 2px;
  transition: all 0.2s;
  font-size: 12px;
}

.action-btn:hover {
  background: #3e3e3e;
  color: #d4d4d4;
}

.panel-content {
  flex: 1;
  overflow-y: auto;
  padding: 16px;
}

.output-content {
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
}

.output-line {
  color: #d4d4d4;
  line-height: 1.5;
  margin-bottom: 2px;
  white-space: pre-wrap;
}

.error-content {
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
}

.problem-block {
  display: flex;
  align-items: flex-start;
  gap: 12px;
  padding: 12px;
  border: 1px solid transparent;
  border-radius: 6px;
  margin-bottom: 12px;
}

.problem-block.error {
  background: #2d1b1b;
  border-color: #5a2d2d;
}

.problem-block.warning {
  background: #2b2b1b;
  border-color: #5a5a2d;
}

.problem-icon {
  font-size: 18px;
  line-height: 1;
  margin-top: 2px;
}

.problem-body {
  flex: 1;
}

.problem-title {
  color: #f48771;
  font-weight: 600;
}

.problem-block.warning .problem-title {
  color: #f9c23c;
}

.problem-meta {
  color: #888;
  display: flex;
  gap: 10px;
  margin-top: 4px;
}

.meta-file {
  color: #569cd6;
}

.problem-time {
  color: #666;
  margin-top: 2px;
}

.no-errors {
  display: flex;
  align-items: center;
  gap: 8px;
  color: #4ec9b0;
  padding: 8px;
}

.success-icon {
  font-size: 16px;
}

.success-banner {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 12px;
  background: #1f2d2a;
  border: 1px solid #2d5a5a;
  border-radius: 6px;
}

.success-text {
  color: #4ec9b0;
}

.terminal-content {
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
  background: #0c0c0c;
  padding: 16px;
  margin: -16px;
  height: calc(100% + 32px);
  color: #cccccc;
}

.terminal-line {
  line-height: 1.5;
  margin-bottom: 2px;
  display: flex;
}

.prompt {
  color: #4ec9b0;
  font-weight: bold;
}

.output-text {
  color: #d4d4d4;
}

.cursor {
  background: #cccccc;
  animation: blink 1s infinite;
}

@keyframes blink {
  0%, 50% {
    opacity: 1;
  }
  51%, 100% {
    opacity: 0;
  }
}

.panel-content::-webkit-scrollbar {
  width: 8px;
}

.panel-content::-webkit-scrollbar-track {
  background: #1e1e1e;
}

.panel-content::-webkit-scrollbar-thumb {
  background: #424242;
  border-radius: 4px;
}

.panel-content::-webkit-scrollbar-thumb:hover {
  background: #4f4f4f;
}
</style>