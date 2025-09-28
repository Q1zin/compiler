import { createApp } from "vue";
import App from "./App.vue";
import "./styles.css";

const app = createApp(App);

// Здесь можно добавить глобальные плагины, например:
// app.use(router)
// app.use(store)

app.mount("#app");