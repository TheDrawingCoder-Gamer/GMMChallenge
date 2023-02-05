pub mod mods;


use futures::future::join_all;
use std::sync;
#[derive(Clone)]
struct ModItem {
    info: mods::ModInfo, 
    selected: bool, 
    disabled: bool,
}
struct GMMApp {
    gorilla_path: String,
    items: Vec<ModItem>,
}


impl egui::Widget for &mut ModItem {
   fn ui(self, ui: &mut egui::Ui) -> egui::Response {
    ui.horizontal(|ui| {
        ui.add_enabled(!self.disabled, egui::Checkbox::new(&mut self.selected, self.info.name.clone()));
        ui.label(self.info.version.clone());
        ui.with_layout(egui::Layout::right_to_left(egui::Align::TOP), |ui| {
            ui.label(self.info.author.clone());
        });
    }).response
   } 
   
}
impl ModItem {
    fn new(info: mods::ModInfo, selected: bool) -> Self {
        Self {
            info,
            selected,
            disabled: false,
        }
    }
}

fn has_dependency(items: &Vec<ModItem>, name: String) -> bool {
    for i in items {
        if i.selected {
            if let Some(dep) = i.info.dependencies.clone() {
                return dep.contains(&name);
            }
        }
    }
    return false;
}
fn render_items(ui: &mut egui::Ui, items: &mut Vec<ModItem>) {
    let is = items.clone();
    for i in &mut *items {
        if !has_dependency(&is, i.info.name.clone()) {
            i.disabled = false;
        }
    }
    let dependencies: Vec<String> = is.iter().filter(|it| it.selected).flat_map(|it| it.info.dependencies.clone()).flatten().collect();

    if !dependencies.is_empty() {
        for i in &mut *items {
            if dependencies.contains(&i.info.name) {
                i.selected = true;
                i.disabled = true;
            }
        }
    }
    for i in items {
        i.ui(ui);
    }
} 
use egui::Widget;
impl eframe::App for GMMApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.vertical(|ui| {
                ui.horizontal(|ui| {
                    ui.label("Gorilla Path");
                    ui.text_edit_singleline(&mut self.gorilla_path);
                });
                ui.vertical(|ui| {
                   render_items(ui, &mut self.items);
                });
                if ui.button("Install").clicked() {
                    // disabled means
                    let infos: Vec<mods::ModInfo> = (*self.items.clone()).iter().map(|it| it.info.clone()).collect();
                    let future = join_all(self.items.iter().filter(|it| it.selected && !it.disabled).map(|it| it.info.clone()).map(|i| {
                        let infos_i = infos.clone();
                        let path = self.gorilla_path.clone();
                        async move {
                            mods::install_async(&infos_i, &i, &path).await
                        }
                    }));
                    smol::spawn(async {
                        future.await;
                    }).detach();
                }
            });
        });
    }
}
impl GMMApp {
    fn new(infos: Vec<mods::ModInfo>) -> Self {
        Self { gorilla_path: "".to_string(), items: infos.iter().map(|it| ModItem::new(it.clone(), false)).collect() }
    }
}

fn fetch_mods() -> Result<Vec<mods::ModInfo>, String> {
    let da_mods = mods::fetch_json();
    if let json::JsonValue::Array(vec) = da_mods {
        Ok(vec.iter().filter_map(|o| {
            if let json::JsonValue::Object(mod_obj) = o {
                mods::make_modinfo(mod_obj)
            } else {
                None
            }
        }).collect())
    } else {
        Err("Invalid modlist".to_string())
    }
}
fn main() -> () {
   let native_options = eframe::NativeOptions::default();
   let mods = fetch_mods().unwrap();
   eframe::run_native("Rusty Gorilla Mod Manager", native_options, Box::new(|_cc| Box::new(GMMApp::new(mods))));
}
