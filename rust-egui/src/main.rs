pub mod mods;
#[macro_use]
pub mod widgets;
pub mod install_data;
use std::sync;

#[derive(Clone)]
struct ModItem {
    info: mods::ModInfo,
    selected: bool,
    disabled: bool,
}
#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
struct GMMApp {
    gorilla_path: String,
    #[serde(skip)]
    items: std::cell::RefCell<Vec<ModItem>>,
    pending_install: bool,
    #[serde(skip)]
    finished_tx: sync::mpsc::Sender<()>,
    #[serde(skip)]
    finished_rx: sync::mpsc::Receiver<()>,
    ignore_dependencies: bool,
    #[serde(skip)]
    status: String,
    #[serde(skip)]
    status_tx: sync::mpsc::Sender<String>,
    #[serde(skip)]
    status_rx: sync::mpsc::Receiver<String>,
    notebook_tab: u64,
}

impl egui::Widget for &mut ModItem {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        ui.horizontal(|ui| {
            ui.add_enabled(
                !self.disabled,
                egui::Checkbox::new(&mut self.selected, self.info.name.clone()),
            );
            ui.label(self.info.version.clone());
            ui.with_layout(egui::Layout::right_to_left(egui::Align::TOP), |ui| {
                ui.label(self.info.author.clone());
            });
        })
        .response
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
    let dependencies: Vec<String> = is
        .iter()
        .filter(|it| it.selected)
        .flat_map(|it| it.info.dependencies.clone())
        .flatten()
        .collect();

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
                    if ui.button("Gorilla Path").clicked() {
                        let file = rfd::FileDialog::new().pick_folder();
                        if let Some(f) = file {
                            self.gorilla_path = f.to_str().unwrap().to_string();
                        }
                    }
                    ui.text_edit_singleline(&mut self.gorilla_path);
                });
                /*
                ui.vertical(|ui| {
                    render_items(ui, self.items.get_mut());
                });
                */
                widgets::Notebook::new(vec![widgets::NotebookPage { name: "Mods".to_string(),
                    widget: Box::new(|ui: &mut egui::Ui| { render_items(ui, &mut *self.items.borrow_mut())})
                }]
                    ,&mut self.notebook_tab);
                match self.status_rx.try_recv() {
                    Ok(s) => self.status = s,
                    Err(_) => (),
                }
                match self.finished_rx.try_recv() {
                    Ok(_) => {
                        self.pending_install = false;
                        self.status = "Finished Install.".to_string();
                    }
                    Err(_) => (),
                }
                ui.horizontal(|ui| {
                    ui.add_enabled_ui(!self.pending_install, |ui| {
                        if ui.button("Install").clicked() {
                            self.pending_install = true;
                            let infos: Vec<mods::ModInfo> = (*self.items.clone().into_inner())
                                .iter()
                                .map(|it| it.info.clone())
                                .collect();
                            let to_install: Vec<mods::ModInfo> = (*self.items.clone().into_inner())
                                .iter()
                                .filter(|it| it.selected && !it.disabled)
                                .map(|it| it.info.clone())
                                .collect();
                            let tx = self.finished_tx.clone();
                            let path = self.gorilla_path.clone();
                            let stx = self.status_tx.clone();
                            smol::spawn(async move {
                                mods::install_mods_async(&infos, &to_install, &path, Some(stx))
                                    .await
                                    .unwrap();
                                tx.send(()).unwrap();
                            })
                            .detach();
                        }
                    });
                    ui.label(self.status.clone());
                });
            });
        });
    }
}
impl Default for GMMApp {
    fn default() -> Self {
        let (tx, rx) = sync::mpsc::channel();
        let (stx, srx) = sync::mpsc::channel();
        Self {
            gorilla_path: "".to_string(),
            items: std::cell::RefCell::new(Vec::new()),
            finished_tx: tx,
            finished_rx: rx,
            pending_install: false,
            ignore_dependencies: false,
            status: "".to_string(),
            status_tx: stx,
            status_rx: srx,
            notebook_tab: 0,
        }
    }
}
impl GMMApp {
    fn new(cc: &eframe::CreationContext<'_>, infos: Vec<mods::ModInfo>) -> Self {
        let mut val = if let Some(storage) = cc.storage {
            eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default()
        } else {
            GMMApp::default()
        };
        val.reload_mods(infos);

        val
    }
    fn reload_mods(&mut self, infos: Vec<mods::ModInfo>) -> () {
        self.items =
            std::cell::RefCell::new(infos
                .iter()
                .map(|it| ModItem::new(it.clone(), false))
                .collect());
    }
}

fn main() -> () {
    let native_options = eframe::NativeOptions::default();
    let mods = mods::fetch_mods().unwrap();
    eframe::run_native(
        "Rusty Gorilla Mod Manager",
        native_options,
        Box::new(|cc| Box::new(GMMApp::new(cc, mods))),
    );
}
