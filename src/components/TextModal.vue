<script setup lang="ts">
import { onMounted, onBeforeUnmount } from 'vue'

defineProps<{
  title: string
  text: string
  show: boolean
}>()

const emit = defineEmits<{
  'close': []
  'insert': []
}>()

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
        <button class="icon-btn" title="Закрыть" @click="emit('close')">✖</button>
      </div>
      <div class="body">
        <pre class="content">{{ text }}</pre>
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
  width: min(800px, 90vw);
  max-height: 80vh;
  background: #1e1e1e;
  border: 1px solid #3e3e3e;
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
  background: #2d2d30;
  border-bottom: 1px solid #3e3e3e;
}

.title {
  margin: 0;
  font-size: 16px;
  color: #d4d4d4;
}

.icon-btn {
  background: none;
  border: none;
  color: #888;
  cursor: pointer;
  font-size: 14px;
}
.icon-btn:hover { color: #d4d4d4; }

.body {
  padding: 16px;
  overflow: auto;
}

.content {
  margin: 0;
  white-space: pre-wrap;
  color: #d4d4d4;
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
  font-size: 13px;
  line-height: 1.5;
}

.footer {
  display: flex;
  gap: 8px;
  justify-content: flex-end;
  padding: 12px 16px;
  border-top: 1px solid #3e3e3e;
}

.btn {
  background: #0e639c;
  color: white;
  border: none;
  padding: 8px 12px;
  border-radius: 4px;
  cursor: pointer;
  font-size: 12px;
}
.btn:hover { background: #1177bb; }

.btn.secondary {
  background: #3e3e3e;
}
.btn.secondary:hover { background: #4a4a4a; }
</style>