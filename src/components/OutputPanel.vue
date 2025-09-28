<script setup lang="ts">
import { ref, computed } from 'vue'

const props = defineProps<{
  output: string
  fontSize: number
}>()

const activeTab = ref('output')

const tabs = [
  { id: 'output', name: 'Вывод', icon: '📄' },
  { id: 'errors', name: 'Ошибки', icon: '⚠️' },
  { id: 'terminal', name: 'Терминал', icon: '💻' }
]

const outputLines = computed(() => {
  if (!props.output) return ['Программа ещё не запускалась...']
  return props.output.split('\n')
})

const clearOutput = () => {
  // Emit event to parent to clear output
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
          :class="{ active: activeTab === tab.id }"
          @click="activeTab = tab.id"
        >
          <span class="tab-icon">{{ tab.icon }}</span>
          <span>{{ tab.name }}</span>
        </button>
      </div>
      
      <div class="panel-actions">
        <button class="action-btn" @click="clearOutput" title="Очистить">
          🗑️
        </button>
        <button class="action-btn" title="Настройки">
          ⚙️
        </button>
      </div>
    </div>
    
    <div class="panel-content">
      <!-- Вывод программы -->
      <div v-if="activeTab === 'output'" class="output-content">
        <div 
          v-for="(line, index) in outputLines" 
          :key="index"
          class="output-line"
          :style="{ fontSize: fontSize + 'px' }"
        >
          {{ line }}
        </div>
      </div>
      
      <!-- Ошибки -->
      <div v-else-if="activeTab === 'errors'" class="error-content">
        <div class="error-message" :style="{ fontSize: fontSize + 'px' }">
          <span class="error-icon">❌</span>
          <div class="error-details">
            <div class="error-title">SyntaxError: Unexpected token</div>
            <div class="error-location">at line 5, column 12</div>
            <div class="error-file">main.js</div>
          </div>
        </div>
        
        <div class="no-errors" :style="{ fontSize: fontSize + 'px' }">
          <span class="success-icon">✅</span>
          <span>Ошибок не найдено</span>
        </div>
      </div>
      
      <!-- Терминал -->
      <div v-else-if="activeTab === 'terminal'" class="terminal-content">
        <div class="terminal-line" :style="{ fontSize: fontSize + 'px' }">
          <span class="prompt">$ </span>
          <span>node main.js</span>
        </div>
        <div class="terminal-line" :style="{ fontSize: fontSize + 'px' }">
          <span class="output-text">Hello, World!</span>
        </div>
        <div class="terminal-line current" :style="{ fontSize: fontSize + 'px' }">
          <span class="prompt">$ </span>
          <span class="cursor">|</span>
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

.panel-actions {
  display: flex;
  gap: 4px;
  padding: 8px;
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

.error-message {
  display: flex;
  align-items: flex-start;
  gap: 12px;
  padding: 12px;
  background: #2d1b1b;
  border: 1px solid #5a2d2d;
  border-radius: 4px;
  margin-bottom: 12px;
}

.error-icon {
  font-size: 16px;
  flex-shrink: 0;
}

.error-details {
  flex: 1;
}

.error-title {
  color: #f48771;
  font-weight: bold;
  margin-bottom: 4px;
}

.error-location {
  color: #888;
  font-size: 12px;
  margin-bottom: 2px;
}

.error-file {
  color: #569cd6;
  font-size: 12px;
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