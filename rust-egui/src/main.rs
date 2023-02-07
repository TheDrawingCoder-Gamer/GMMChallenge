#[macro_use]
pub mod widgets;
use std::{sync::{self, Arc, Mutex}, path::Path};
use gmmlib_rs;
use itertools::Itertools;
#[derive(Clone)]
struct ModItem {
    info: gmmlib_rs::ModInfo,
    selected: bool,
    disabled: bool,
}

impl gmmlib_rs::ModItem for ModItem {
    fn selected(&self) -> bool {
        self.selected
    }
    fn disabled(&self) -> bool {
        self.disabled
    }
    fn identifier(&self) -> String {
        self.info.name.clone()
    }
    fn dependencies(&self) -> Vec<String> {
        self.info.dependencies.clone().unwrap_or(Vec::new())
    }
    fn set_selected(&mut self, selected: bool) -> () {
        self.selected = selected;
    }
    fn set_disabled(&mut self, disabled: bool) -> () {
        self.disabled = disabled;
    }
}
struct GroupWidget<'a> {
    name: String,
    items: Vec<&'a mut ModItem>,
}

impl egui::Widget for GroupWidget<'_> {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        ui.label(self.name);
        ui.vertical(|ui| {
            for item in self.items {
               ui.add(ModWidget { item });
            }
        }).response
    }
}

#[derive(serde::Deserialize, serde::Serialize)]
enum Tabs {
    Mods,
    Utils,
}

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
struct GMMApp {
    gorilla_path: String,
    #[serde(skip)]
    items: std::cell::RefCell<Vec<ModItem>>,
    #[serde(skip)]
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
    #[serde(skip)]
    notebook_tab: Tabs,
    version_data: sync::Arc<sync::Mutex<gmmlib_rs::InstallDatas>>,
    #[serde(skip)]
    in_modal: bool,
    #[serde(skip)]
    accepted_nuke: bool,
    #[serde(skip)]
    groups: Vec<gmmlib_rs::Group>,
}

struct ModWidget<'a> {
    item: &'a mut ModItem
}
impl<'a> egui::Widget for ModWidget<'a> {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let item = self.item;
        ui.horizontal(|ui| {
            ui.add_enabled(
                !item.disabled,
                egui::Checkbox::new(&mut item.selected, item.info.name.clone()),
            );
            ui.label(item.info.version.clone());
            ui.with_layout(egui::Layout::right_to_left(egui::Align::TOP), |ui| {
                ui.label(item.info.author.clone());
            });
        })
        .response
    }
}
impl ModItem {
    fn new(info: gmmlib_rs::ModInfo, selected: bool) -> Self {
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
fn render_items(ui: &mut egui::Ui, items: &mut Vec<ModItem>, ignore_deps: bool, groups: &Vec<gmmlib_rs::Group>) {

    let is = items.clone();
    for i in &mut *items {
        if ignore_deps || !has_dependency(&is, i.info.name.clone()) {
            i.disabled = false;
        }
    }
    if !ignore_deps {
        gmmlib_rs::handle_dependencies(items);
    }
    items.sort_by_cached_key(|it| groups.iter().find(|ti| it.info.group == ti.name).unwrap().rank);
    let groups = Itertools::group_by(items.into_iter(), |it| it.info.group.clone());
    for (k, items) in groups.into_iter() {
       ui.add(GroupWidget { name: k, items: items.collect_vec() });
    }

}

impl eframe::App for GMMApp {
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, self);
        
    }
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if self.in_modal {
            egui::Window::new("Delete Mods?").resizable(false).collapsible(false).show(ctx, |ui| {
                ui.vertical(|ui| {
                    ui.label("Are you SURE you want to remove all mods?");
                    ui.label("This is irreversable!");
                    ui.horizontal(|ui| {
                        if ui.button("No").clicked() {
                            self.in_modal = false;
                        }
                        if ui.button("Yes").clicked() {
                            self.accepted_nuke = true;
                            self.in_modal = false;
                        }
                    });
                });
            });
        }
        if self.accepted_nuke {
            self.accepted_nuke = false;
            let mut res = self.version_data.lock().unwrap();
            let data: Vec<String> = (*res).clone()
                .mods
                .iter()
                .map(|(_k, v)| v.structure.clone())
                .flatten()
                .collect();
            let g_path = Path::new(&self.gorilla_path);
            for file in data {
                let path = g_path.join(&file);
                if path.is_file() {
                    std::fs::remove_file(&path).unwrap();
                } else {
                    println!("{} isn't a file", path.display());
                }
            }
            *res = gmmlib_rs::InstallDatas::default();
            self.status = "Removed ALL mods".to_string();
        }
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.add_enabled_ui(!self.in_modal, |ui| {
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
                    ui.horizontal(|ui| {
                        if ui.button("Mods").clicked() {
                            self.notebook_tab = Tabs::Mods; 
                        } 
                        if ui.button("Utils").clicked() {
                            self.notebook_tab = Tabs::Utils;
                        }
                    });
                    match self.notebook_tab {
                        Tabs::Mods => {
                            ui.vertical(|ui| { render_items(ui, self.items.get_mut(), self.ignore_dependencies, &self.groups); });
                        },
                        Tabs::Utils => {
                            if ui.button("Delete ALL Mods").clicked() {
                                self.in_modal = true;
                            }
                        }
                    }
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
                    ui.with_layout(egui::Layout::bottom_up(egui::Align::Min), |ui| {
                        ui.horizontal(|ui| {
                            ui.checkbox(&mut self.ignore_dependencies, "Ignore Dependencies");
                            ui.add_enabled_ui(!self.pending_install, |ui| {
                                if ui.button("Install").clicked() {
                                    self.pending_install = true;
                                    let infos: Vec<gmmlib_rs::ModInfo> = (*self.items.clone().into_inner())
                                        .iter()
                                        .map(|it| it.info.clone())
                                        .collect();
                                    let to_install: Vec<gmmlib_rs::ModInfo> = (*self.items.clone().into_inner())
                                        .iter()
                                        .filter(|it| it.selected && !it.disabled)
                                        .map(|it| it.info.clone())
                                        .collect();
                                    let tx = self.finished_tx.clone();
                                    let path = self.gorilla_path.clone();
                                    let stx = self.status_tx.clone();
                                    let v_data = self.version_data.clone();
                                    let ignore_deps = self.ignore_dependencies;
                                    smol::spawn(async move {
                                        let mut res = v_data.lock().unwrap();
                                        let data = gmmlib_rs::install_mods(&infos, &to_install, &path, &|it: &str| stx.send(it.to_string()).unwrap(), &*res, ignore_deps)
                                            .unwrap();
                                        *res = data;
                                        tx.send(()).unwrap();
                                    })
                                    .detach();
                                }
                            });
                            if ui.button("Clear selections").clicked() {
                                for item in self.items.get_mut() {
                                    item.selected = false;
                                    item.disabled = false;
                                }
                            }
                            ui.label(self.status.clone());
                        });
                    });
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
            notebook_tab: Tabs::Mods,
            version_data: Arc::new(Mutex::new(gmmlib_rs::InstallDatas::default())),
            in_modal: false,
            accepted_nuke: false,
            groups: Vec::new(),
        }
    }
}
impl GMMApp {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let mut val = if let Some(storage) = cc.storage {
            eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default()
        } else {
            GMMApp::default()
        };
        val.reload_mods(gmmlib_rs::get_mmm_mods().unwrap());
        val.groups = gmmlib_rs::get_mmm_groups().unwrap();
        val
    }
    fn reload_mods(&mut self, infos: Vec<gmmlib_rs::ModInfo>) -> () {
        self.items =
            std::cell::RefCell::new(infos
                .iter()
                .map(|it| ModItem::new(it.clone(), false))
                .collect());
    }

}

fn main() -> () {
    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "Rusty Gorilla Mod Manager",
        native_options,
        Box::new(|cc| Box::new(GMMApp::new(cc))),
    );
}
