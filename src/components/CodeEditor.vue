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
const highlightLayer = ref<HTMLPreElement>()

const computedValue = computed({
  get: () => props.modelValue,
  set: (value) => emit('update:modelValue', value)
})

const lines = computed(() => {
  const lineCount = props.modelValue.split('\n').length
  return Array.from({ length: Math.max(lineCount, 2) }, (_, i) => i + 1)
})

const escapeHtml = (text: string) =>
  text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#039;')

const highlight = (input: string) => {
  const out: string[] = []
  let i = 0

  const push = (cls: string | null, raw: string) => {
    const safe = escapeHtml(raw)
    if (!cls) out.push(safe)
    else out.push(`<span class="${cls}">${safe}</span>`)
  }

  while (i < input.length) {
    const ch = input[i]

    if (ch === '\n') {
      out.push('\n')
      i += 1
      continue
    }

    if (ch === ' ' || ch === '\t' || ch === '\r') {
      let j = i + 1
      while (j < input.length && (input[j] === ' ' || input[j] === '\t' || input[j] === '\r')) j += 1
      push(null, input.slice(i, j))
      i = j
      continue
    }

    if (ch === '.') {
      const m = input.slice(i).match(/^\.(TRUE|FALSE|AND|OR|GT|LT)\./i)
      if (m) {
        push('tok-kw', m[0])
        i += m[0].length
        continue
      }
      push('tok-punct', '.')
      i += 1
      continue
    }

    if (ch === '(' || ch === ')') {
      push('tok-paren', ch)
      i += 1
      continue
    }

    if (ch >= '0' && ch <= '9') {
      let j = i + 1
      while (j < input.length && input[j] >= '0' && input[j] <= '9') j += 1
      push('tok-num', input.slice(i, j))
      i = j
      continue
    }

    if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch === '_') {
      let j = i + 1
      while (j < input.length) {
        const c = input[j]
        const isAz = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
        const is09 = c >= '0' && c <= '9'
        if (isAz || is09 || c === '_') j += 1
        else break
      }
      push('tok-ident', input.slice(i, j))
      i = j
      continue
    }

    push('tok-err', ch)
    i += 1
  }

  return out.join('')
}

const highlightedHtml = computed(() => highlight(props.modelValue))

const handleScroll = (event: Event) => {
  if (lineNumbers.value && event.target) {
    const target = event.target as HTMLTextAreaElement
    lineNumbers.value.scrollTop = target.scrollTop
    if (highlightLayer.value) {
      highlightLayer.value.scrollTop = target.scrollTop
      highlightLayer.value.scrollLeft = target.scrollLeft
    }
  }
}

const handleInput = () => {
  nextTick(() => {
    if (textareaRef.value && lineNumbers.value) {
      lineNumbers.value.scrollTop = textareaRef.value.scrollTop
      if (highlightLayer.value) {
        highlightLayer.value.scrollTop = textareaRef.value.scrollTop
        highlightLayer.value.scrollLeft = textareaRef.value.scrollLeft
      }
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
      
      <div class="code-area">
        <pre
          ref="highlightLayer"
          class="highlight-layer"
          :style="{ fontSize: fontSize + 'px' }"
          aria-hidden="true"
          v-html="highlightedHtml"
        ></pre>
        <textarea
          ref="textareaRef"
          v-model="computedValue"
          class="code-textarea"
          :style="{ fontSize: fontSize + 'px' }"
          wrap="off"
          spellcheck="false"
          @scroll="handleScroll"
          @input="handleInput"
          @keydown="handleTab"
          placeholder="Начните писать код здесь..."
        />
      </div>
    </div>
  </div>
</template>

<style scoped>
.code-editor {
  display: flex;
  flex-direction: column;
  height: 100%;
  background: var(--bg-primary);
}

.editor-content {
  flex: 1;
  display: flex;
  overflow: hidden;
}

.code-area {
  position: relative;
  flex: 1;
  overflow: hidden;
}

.line-numbers {
  background: var(--bg-tertiary);
  color: var(--text-muted);
  padding: 16px 8px;
  text-align: right;
  user-select: none;
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
  line-height: 1.5;
  min-width: 50px;
  border-right: 1px solid var(--border-color);
  overflow: hidden;
}

.line-number {
  height: 1.5em;
  white-space: nowrap;
}

.code-textarea {
  position: relative;
  z-index: 2;
  width: 100%;
  height: 100%;
  background: transparent;
  color: transparent;
  caret-color: var(--text-primary);
  border: none;
  outline: none;
  padding: 16px;
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
  font-weight: 700;
  line-height: 1.5;
  resize: none;
  tab-size: 2;
  white-space: pre;
  overflow-wrap: normal;
  overflow-x: auto;
}

.highlight-layer {
  position: absolute;
  z-index: 1;
  inset: 0;
  width: 100%;
  height: 100%;
  margin: 0;
  padding: 16px;
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
  line-height: 1.5;
  white-space: pre;
  overflow: auto;
  background: var(--bg-primary);
  color: var(--text-primary);
  pointer-events: none;
  tab-size: 2;
}

.highlight-layer :deep(.tok-kw) {
  color: #c586c0;
  font-weight: 600;
}

.highlight-layer :deep(.tok-ident) {
  color: #9cdcfe;
}

.highlight-layer :deep(.tok-num) {
  color: #b5cea8;
}

.highlight-layer :deep(.tok-paren) {
  color: #ffd700;
}

.highlight-layer :deep(.tok-punct) {
  color: #808080;
}

.highlight-layer :deep(.tok-err) {
  color: #f44747;
  text-decoration: wavy underline;
}

.code-textarea::placeholder {
  color: var(--text-muted);
  font-style: italic;
}

.code-textarea::-webkit-scrollbar {
  width: 12px;
  height: 12px;
}

.code-textarea::-webkit-scrollbar-track { background: var(--bg-primary); }

.code-textarea::-webkit-scrollbar-thumb {
  background: #424242;
  border-radius: 6px;
}

.code-textarea::-webkit-scrollbar-thumb:hover {
  background: #4f4f4f;
}

.code-textarea::-webkit-scrollbar-corner { background: var(--bg-primary); }
</style>