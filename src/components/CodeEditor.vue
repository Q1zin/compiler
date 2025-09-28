<script setup lang="ts">
import { ref, computed, nextTick } from 'vue'

const props = defineProps<{
  modelValue: string
  fontSize: number
}>()

const emit = defineEmits<{
  'update:modelValue': [value: string]
}>()

const textareaRef = ref<HTMLTextAreaElement>()
const lineNumbers = ref<HTMLDivElement>()

const computedValue = computed({
  get: () => props.modelValue,
  set: (value) => emit('update:modelValue', value)
})

const lines = computed(() => {
  const lineCount = props.modelValue.split('\n').length
  return Array.from({ length: Math.max(lineCount, 20) }, (_, i) => i + 1)
})

const handleScroll = (event: Event) => {
  if (lineNumbers.value && event.target) {
    const target = event.target as HTMLTextAreaElement
    lineNumbers.value.scrollTop = target.scrollTop
  }
}

const handleInput = () => {
  nextTick(() => {
    if (textareaRef.value && lineNumbers.value) {
      lineNumbers.value.scrollTop = textareaRef.value.scrollTop
    }
  })
}

const handleTab = (event: KeyboardEvent) => {
  if (event.key === 'Tab') {
    event.preventDefault()
    const target = event.target as HTMLTextAreaElement
    const start = target.selectionStart
    const end = target.selectionEnd
    const value = target.value
    const newValue = value.substring(0, start) + '  ' + value.substring(end)
    emit('update:modelValue', newValue)
    
    nextTick(() => {
      target.selectionStart = target.selectionEnd = start + 2
    })
  }
}
</script>

<template>
  <div class="code-editor">
    <div class="editor-header">
      <span class="file-name">test.txt</span>
    </div>
    
    <div class="editor-content">
      <div 
        ref="lineNumbers" 
        class="line-numbers"
        :style="{ fontSize: fontSize + 'px' }"
      >
        <div 
          v-for="line in lines" 
          :key="line" 
          class="line-number"
        >
          {{ line }}
        </div>
      </div>
      
      <textarea
        ref="textareaRef"
        v-model="computedValue"
        class="code-textarea"
        :style="{ fontSize: fontSize + 'px' }"
        spellcheck="false"
        @scroll="handleScroll"
        @input="handleInput"
        @keydown="handleTab"
        placeholder="// Начните писать код здесь..."
      />
    </div>
  </div>
</template>

<style scoped>
.code-editor {
  display: flex;
  flex-direction: column;
  height: 100%;
  background: #1e1e1e;
}

.editor-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  background: #2d2d30;
  padding: 8px 16px;
  border-bottom: 1px solid #3e3e3e;
  font-size: 12px;
}

.file-name {
  color: #d4d4d4;
  font-weight: 500;
}

.editor-actions {
  display: flex;
  gap: 8px;
}

.action-btn {
  background: none;
  border: none;
  color: #888;
  cursor: pointer;
  font-size: 12px;
  padding: 2px;
  border-radius: 2px;
  transition: all 0.2s;
}

.action-btn:hover {
  background: #3e3e3e;
  color: #d4d4d4;
}

.editor-content {
  flex: 1;
  display: flex;
  overflow: hidden;
}

.line-numbers {
  background: #252526;
  color: #858585;
  padding: 16px 8px;
  text-align: right;
  user-select: none;
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
  line-height: 1.5;
  min-width: 50px;
  border-right: 1px solid #3e3e3e;
  overflow: hidden;
}

.line-number {
  height: 1.5em;
  white-space: nowrap;
}

.code-textarea {
  flex: 1;
  background: #1e1e1e;
  color: #d4d4d4;
  border: none;
  outline: none;
  padding: 16px;
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
  line-height: 1.5;
  resize: none;
  tab-size: 2;
  white-space: pre;
  overflow-wrap: normal;
  overflow-x: auto;
}

.code-textarea::placeholder {
  color: #6a6a6a;
  font-style: italic;
}

.code-textarea::-webkit-scrollbar {
  width: 12px;
  height: 12px;
}

.code-textarea::-webkit-scrollbar-track {
  background: #1e1e1e;
}

.code-textarea::-webkit-scrollbar-thumb {
  background: #424242;
  border-radius: 6px;
}

.code-textarea::-webkit-scrollbar-thumb:hover {
  background: #4f4f4f;
}

.code-textarea::-webkit-scrollbar-corner {
  background: #1e1e1e;
}
</style>