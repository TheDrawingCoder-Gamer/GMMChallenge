#[macro_export]
macro_rules! pages {
    ($ ($x:expr => $y:expr),* ) => {
        vec![$(widgets::NotebookPage { name: $x.to_string(), widget: Box::new($y)},)*]
    };
}

pub struct NotebookPage {
    pub name: String,
    pub widget: Box<dyn FnOnce(&mut egui::Ui)>
}
pub struct Notebook<'a> {
    pub pages: Vec<NotebookPage>,
    pub selected: &'a mut u64
}

impl<'a> Notebook<'a> {
    pub fn new(pages: Vec<NotebookPage>, selected: &'a mut u64) -> Self {
        Self {pages, selected}
    }
}

impl<'a> egui::Widget for Notebook<'a> {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        ui.vertical(|ui| {
            let mut rendered_page: Option<Box<dyn FnOnce(&mut egui::Ui) -> ()>> = None;
            ui.horizontal(|ui| {
                let mut i = 0;
                for NotebookPage{name, widget} in self.pages {
                    if ui.button(name.clone()).clicked() {
                        *self.selected = i
                    }
                    if i == *self.selected {
                        rendered_page = Some(widget);
                    }
                    i += 1;
                }
            });
            if let Some(page) = rendered_page {
                page(ui);
            }
        }).response
    }
}

// FIXME
pub fn notebook(
    ui: &mut egui::Ui,
    pages: Vec<(String, Box<dyn FnOnce(&mut egui::Ui) -> ()>)>,
    selected: &mut u64,
) -> () {
    ui.vertical(|ui| {
        let mut rendered_page: Option<Box<dyn FnOnce(&mut egui::Ui) -> ()>> = None;
        ui.horizontal(|ui| {
            let mut i = 0;
            for (name, page) in pages {
                if ui.button(name.clone()).clicked() {
                    *selected = i
                }
                if i == *selected {
                    rendered_page = Some(page);
                }
                i += 1;
            }
        });
        if let Some(page) = rendered_page {
            page(ui);
        }
    });
}
