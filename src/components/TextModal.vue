<script setup lang="ts">
import { ref, onMounted, onBeforeUnmount } from 'vue'

defineProps<{
  title: string
  text: string
  show: boolean
}>()

const emit = defineEmits<{
  'close': []
  'insert': []
}>()

const fontSize = ref(14)

const increaseFontSize = () => {
  fontSize.value = Math.min(24, fontSize.value + 1)
}

const decreaseFontSize = () => {
  fontSize.value = Math.max(10, fontSize.value - 1)
}

const onKey = (e: KeyboardEvent) => {
  if (e.key === 'Escape') emit('close')
}

onMounted(() => document.addEventListener('keydown', onKey))
onBeforeUnmount(() => document.removeEventListener('keydown', onKey))
</script>

<template>
  <div v-if="show" class="overlay" @click.self="emit('close')">
    <div class="modal">
      <div class="header">
        <h3 class="title">{{ title }}</h3>
        <div class="header-controls">
          <div class="font-controls">
            <button class="font-btn" @click="decreaseFontSize" title="Уменьшить шрифт">A-</button>
            <span class="font-size-label">{{ fontSize }}px</span>
            <button class="font-btn" @click="increaseFontSize" title="Увеличить шрифт">A+</button>
          </div>
          <button class="icon-btn" title="Закрыть" @click="emit('close')">✖</button>
        </div>
      </div>
      <div class="body">
        <pre class="content" :style="{ fontSize: fontSize + 'px' }">{{ text }}</pre>
      </div>
      <div class="footer">
        <button class="btn" @click="emit('insert')">Вставить в редактор</button>
        <button class="btn secondary" @click="emit('close')">Закрыть</button>
      </div>
    </div>
  </div>
</template>

<style scoped>
.overlay {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.5);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
}

.modal {
  width: min(1100px, 95vw);
  max-height: 90vh;
  background: var(--bg-primary);
  border: 1px solid var(--border-color);
  border-radius: 8px;
  box-shadow: 0 10px 30px rgba(0,0,0,0.4);
  display: flex;
  flex-direction: column;
}

.header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 12px 16px;
  background: var(--bg-secondary);
  border-bottom: 1px solid var(--border-color);
}

.header-controls {
  display: flex;
  align-items: center;
  gap: 16px;
}

.font-controls {
  display: flex;
  align-items: center;
  gap: 8px;
}

.font-btn {
  background: var(--bg-primary);
  border: 1px solid var(--border-color);
  color: var(--text-primary);
  padding: 4px 8px;
  border-radius: 4px;
  cursor: pointer;
  font-size: 12px;
  font-weight: bold;
}

.font-btn:hover {
  background: var(--accent);
  color: white;
}

.font-size-label {
  color: var(--text-muted);
  font-size: 12px;
  min-width: 40px;
  text-align: center;
}

.title { margin: 0; font-size: 16px; color: var(--text-primary); }

.icon-btn {
  background: none;
  border: none;
  color: var(--text-muted);
  cursor: pointer;
  font-size: 14px;
}
.icon-btn:hover { color: var(--text-primary); }

.body {
  padding: 20px;
  overflow: auto;
  flex: 1;
}

.content {
  margin: 0;
  white-space: pre-wrap;
  color: var(--text-primary);
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
  line-height: 1.6;
}

.footer {
  display: flex;
  gap: 8px;
  justify-content: flex-end;
  padding: 12px 16px;
  border-top: 1px solid var(--border-color);
}

.btn {
  background: var(--accent);
  color: white;
  border: none;
  padding: 8px 12px;
  border-radius: 4px;
  cursor: pointer;
  font-size: 12px;
}
.btn:hover { background: var(--accent-hover); }

.btn.secondary { background: var(--border-color); }
.btn.secondary:hover { background: var(--accent-hover); }
</style>