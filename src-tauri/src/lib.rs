pub mod logic_parser;

#[tauri::command]
fn greet(name: &str) -> String {
    format!("Hello, {}! You've been greeted from Rust!", name)
}

#[tauri::command]
fn validate_expression(input: String) -> logic_parser::ValidationResult {
    logic_parser::validate_expression(&input)
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    tauri::Builder::default()
        .plugin(tauri_plugin_fs::init())
        .plugin(tauri_plugin_dialog::init())
        .plugin(tauri_plugin_opener::init())
    .invoke_handler(tauri::generate_handler![greet, validate_expression])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
