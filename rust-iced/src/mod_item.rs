use iced::{widget::{row, text, horizontal_space, self}, Element};
use iced_lazy::Component;
// Sinner!
#[derive(Clone)]
pub struct ModItem<'a, Message> {
    pub info: gmmlib_rs::ModInfo,
    pub selected: bool,
    pub disabled: bool,
    on_selected_toggle: std::rc::Rc<dyn Fn(bool) -> Message + 'a>,
}

impl<'a, Message> ModItem<'a, Message> {
    pub fn new<F: Fn(bool) -> Message + 'a>(info: gmmlib_rs::ModInfo, on_selected_toggle: F) -> Self {
        Self {
            info,
            on_selected_toggle: std::rc::Rc::new(on_selected_toggle),
            selected: false,
            disabled: false,
        }
    }
}

pub enum Event {
    SelectedToggle(bool)
}
impl<'a, Message, Renderer> Component<Message, Renderer> for ModItem<'a, Message> 
where 
        Renderer: iced_native::text::Renderer + 'static,
        Renderer::Theme: widget::text::StyleSheet
            + widget::checkbox::StyleSheet {
    type State = ();
    type Event = Event;

    fn view(&self, _state: &Self::State) -> iced_native::Element<'_, Self::Event, Renderer> {
        row![
            widget::Checkbox::new(self.info.name.clone(), self.selected, Event::SelectedToggle),
            text(self.info.version.clone()),
            horizontal_space(iced::Length::Fill),
            text(self.info.author.clone()),
        ].into()
    }

    fn update(
            &mut self,
            state: &mut Self::State,
            event: Self::Event,
        ) -> Option<Message> {
        match event {
            Event::SelectedToggle(s) => {
                self.selected = s;
                Some((self.on_selected_toggle)(s))
            }
        }
    }
}

impl<'a, Message, Renderer> From<ModItem<'a, Message>> for Element<'a, Message, Renderer> 
where
    Renderer: iced_native::text::Renderer + 'static,
    Renderer::Theme: widget::text::StyleSheet
        + widget::checkbox::StyleSheet, {
    fn from(value: ModItem<'a, Message>) -> Self {
        iced_lazy::component(value)            
    }        
}
