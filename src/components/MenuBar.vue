<script setup lang="ts">
import { ref } from 'vue'

const emit = defineEmits<{
  'file-action': [action: string]
  'text-action': [action: string]
  'run-code': []
  'font-size-change': [change: number]
}>()

const activeMenu = ref<string | null>(null)

const toggleMenu = (menu: string) => {
  activeMenu.value = activeMenu.value === menu ? null : menu
}

const closeMenu = () => {
  activeMenu.value = null
}

const handleAction = (type: string, action: string) => {
  closeMenu()
  if (type === 'file') {
    emit('file-action', action)
  } else if (type === 'text') {
    emit('text-action', action)
  }
}

const handleRun = () => {
  closeMenu()
  emit('run-code')
}

const handleFontChange = (change: number) => {
  emit('font-size-change', change)
}
</script>

<template>
  <div class="menu-bar">
    <div class="menu-items">
      <!-- Файл -->
      <div class="menu-item" @click="toggleMenu('file')">
        <span>Файл</span>
        <div v-if="activeMenu === 'file'" class="dropdown" @click.stop>
          <div class="dropdown-item" @click="handleAction('file', 'new')">
            <span>Создать</span>
            <span class="shortcut">Ctrl+N</span>
          </div>
          <div class="dropdown-item" @click="handleAction('file', 'open')">
            <span>Открыть</span>
            <span class="shortcut">Ctrl+O</span>
          </div>
          <div class="dropdown-separator"></div>
          <div class="dropdown-item" @click="handleAction('file', 'save')">
            <span>Сохранить</span>
            <span class="shortcut">Ctrl+S</span>
          </div>
          <div class="dropdown-item" @click="handleAction('file', 'save-as')">
            <span>Сохранить как...</span>
            <span class="shortcut">Ctrl+Shift+S</span>
          </div>
          <div class="dropdown-separator"></div>
          <div class="dropdown-item" @click="handleAction('file', 'close')">
            <span>Закрыть вкладку</span>
            <span class="shortcut">Ctrl+W</span>
          </div>
          <div class="dropdown-item" @click="handleAction('file', 'close-all')">
            <span>Закрыть все</span>
            <span class="shortcut">Ctrl+Shift+W</span>
          </div>
          <div class="dropdown-item" @click="handleAction('file', 'close-others')">
            <span>Закрыть остальные</span>
          </div>
        </div>
      </div>

      <!-- Текст -->
      <div class="menu-item" @click="toggleMenu('text')">
        <span>Текст</span>
        <div v-if="activeMenu === 'text'" class="dropdown" @click.stop>
          <div class="dropdown-item" @click="handleAction('text', 'task')">
            <span>Постановка задачи</span>
          </div>
          <div class="dropdown-item" @click="handleAction('text', 'bibliography')">
            <span>Список литературы</span>
          </div>
          <div class="dropdown-item" @click="handleAction('text', 'source-code')">
            <span>Исходный код программы</span>
          </div>
        </div>
      </div>

      <!-- Пуск -->
      <div class="menu-item run-button" @click="handleRun">
        <span>▶ Пуск</span>
      </div>

      <!-- Вид -->
      <div class="menu-item" @click="toggleMenu('view')">
        <span>Вид</span>
        <div v-if="activeMenu === 'view'" class="dropdown" @click.stop>
          <div class="dropdown-header">Размер текста</div>
          <div class="dropdown-item font-controls">
            <span>Окно кода</span>
            <div class="font-buttons">
              <button @click="handleFontChange(-1)" class="font-btn">A-</button>
              <button @click="handleFontChange(1)" class="font-btn">A+</button>
            </div>
          </div>
          <div class="dropdown-item font-controls">
            <span>Окно вывода</span>
            <div class="font-buttons">
              <button @click="handleFontChange(-1)" class="font-btn">A-</button>
              <button @click="handleFontChange(1)" class="font-btn">A+</button>
            </div>
          </div>
          <div class="dropdown-separator"></div>
          <div class="dropdown-header">Вкладки</div>
          <div class="dropdown-item" @click="handleAction('file', 'new')">
            <span>Новая вкладка</span>
            <span class="shortcut">Ctrl+T</span>
          </div>
        </div>
      </div>
    </div>
  </div>

  <!-- Overlay для закрытия меню -->
  <div v-if="activeMenu" class="overlay" @click="closeMenu"></div>
</template>

<style scoped>
.menu-bar {
  background: #2d2d30;
  border-bottom: 1px solid #3e3e3e;
  padding: 0;
  position: relative;
  z-index: 1000;
}

.menu-items {
  display: flex;
  align-items: center;
}

.menu-item {
  position: relative;
  padding: 8px 16px;
  cursor: pointer;
  user-select: none;
  transition: background-color 0.2s;
  font-size: 13px;
  color: #d4d4d4;
}

.menu-item:hover {
  background: #3e3e3e;
}

.run-button {
  background: #0e639c;
  color: white;
  margin-left: 8px;
  border-radius: 3px;
}

.run-button:hover {
  background: #1177bb;
}

.dropdown {
  position: absolute;
  top: 100%;
  left: 0;
  background: #2d2d30;
  border: 1px solid #3e3e3e;
  border-radius: 3px;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
  min-width: 220px;
  z-index: 1001;
}

.dropdown-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 8px 16px;
  cursor: pointer;
  transition: background-color 0.2s;
  color: #d4d4d4;
}

.dropdown-item:hover {
  background: #3e3e3e;
}

.dropdown-header {
  padding: 8px 16px;
  font-size: 11px;
  color: #888;
  text-transform: uppercase;
  border-bottom: 1px solid #3e3e3e;
  background: #252526;
}

.dropdown-separator {
  height: 1px;
  background: #3e3e3e;
  margin: 4px 0;
}

.shortcut {
  font-size: 11px;
  color: #888;
}

.font-controls {
  justify-content: space-between;
}

.font-buttons {
  display: flex;
  gap: 4px;
}

.font-btn {
  background: #0e639c;
  color: white;
  border: none;
  padding: 2px 6px;
  border-radius: 2px;
  cursor: pointer;
  font-size: 10px;
  transition: background-color 0.2s;
}

.font-btn:hover {
  background: #1177bb;
}

.overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  z-index: 999;
}
</style>