<script setup lang="ts">
import { computed } from 'vue'
import type { ProgramOutput } from '../types'

const props = defineProps<{
  items?: ProgramOutput[]
  fontSize: number
}>()

const emit = defineEmits<{
  'clear-output': []
}>()

const problems = computed(() => (props.items ?? []).filter(i => i.type === 'error'))
const hasProblems = computed(() => problems.value.length > 0)

const clearOutput = () => {
  emit('clear-output')
}
</script>

<template>
  <div class="output-panel">
    <div class="panel-header">
      <div class="tabs">
        <div class="tab active">
          <span class="tab-icon">⚠️</span>
          <span>Ошибки</span>
        </div>
      </div>
      <div style="margin-left:auto; padding-right:8px;">
        <button class="action-btn" @click="clearOutput">Очистить</button>
      </div>
    </div>
    
    <div class="panel-content">
      <div class="error-content">
        <div v-if="hasProblems">
          <div 
            v-for="(p, idx) in problems" 
            :key="idx"
            class="problem-block error"
          >
            <div class="problem-icon">❌</div>
            <div class="problem-body">
              <div class="problem-title" :style="{ fontSize: (fontSize + 1) + 'px' }">
                {{ p.message }}
              </div>
              <div v-if="p.line || p.column || p.file" class="problem-meta" :style="{ fontSize: (fontSize - 2) + 'px' }">
                <div v-if="p.file" class="meta-row">
                  <span class="meta-file">{{ p.file }}</span>
                </div>
                <div v-if="p.line || p.column" class="meta-row">
                  <span v-if="p.line">line: {{ p.line }}</span>
                  <span v-if="p.column">column: {{ p.column }}</span>
                </div>
              </div>
              <div class="problem-time" :style="{ fontSize: (fontSize - 3) + 'px' }">{{ new Date(p.timestamp).toLocaleTimeString() }}</div>
            </div>
          </div>
        </div>
        <div v-else class="no-errors" :style="{ fontSize: fontSize + 'px' }">
          <span class="success-icon">✅</span>
          <span>Ошибок нет</span>
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
  background: var(--bg-primary);
}

.panel-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  background: var(--bg-secondary);
  border-bottom: 1px solid var(--border-color);
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
  background: var(--border-color);
  color: var(--text-primary);
}

.tab.active {
  color: var(--text-primary);
  border-bottom-color: #0e639c;
  background: var(--bg-primary);
}

.tab-icon {
  font-size: 14px;
}

.action-btn {
  background: none;
  border: none;
  color: var(--text-muted);
  cursor: pointer;
  padding: 4px 8px;
  border-radius: 2px;
  transition: all 0.2s;
  font-size: 12px;
}

.action-btn:hover {
  background: var(--border-color);
  color: var(--text-primary);
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
  color: var(--text-primary);
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
  color: var(--text-muted);
  display: flex;
  flex-direction: column;
  gap: 2px;
  margin-top: 4px;
}

.meta-file {
  color: #569cd6;
}

.meta-row {
  display: flex;
  gap: 10px;
  flex-wrap: wrap;
}

.problem-time {
  color: #666;
  margin-top: 2px;
}

.no-errors {
  display: flex;
  align-items: center;
  gap: 8px;
  color: var(--success);
  padding: 8px;
}

.success-icon {
  font-size: 16px;
  line-height: 1px;
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
  color: var(--success);
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