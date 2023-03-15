use iced::widget::{column, self};
use crate::mod_item;
pub struct GroupWidget<'a, Message> {
    pub name: String,
    infos: Vec<crate::mod_item::ModItem<'a, Event>>, 
    on_toggled_fn: Box<dyn Fn(usize, bool) -> Message + 'a>,
}

#[derive(Clone)]
pub enum Event {
    SelectedToggle(usize, bool)
}
use iced_lazy::Component;
impl<'a, Message, Renderer> Component<Message, Renderer> for GroupWidget<'a, Message>
    where
        Renderer: iced_native::text::Renderer + 'static, 
        Renderer::Theme: widget::text::StyleSheet
            + widget::checkbox::StyleSheet, {
    type State = ();
    type Event = Event;
    fn view(&self, _state: &Self::State) -> iced_native::Element<'_, Self::Event, Renderer> {
        let infos_view: Vec<iced_native::Element<'_, Self::Event, Renderer>> = self.infos
            .iter()
            .map(|it| it.clone().into())
            .collect();
        column![
            widget::text(self.name.clone()),
            column(infos_view)
        ].into()

    }
    fn update(
            &mut self,
            _state: &mut Self::State,
            event: Self::Event,
        ) -> Option<Message> {
        match event {
            Event::SelectedToggle(idx, s) => {
                let info: &mut mod_item::ModItem<'a, Self::Event> = self.infos.get_mut(idx).unwrap();
                Component::<Event, Renderer>::update(info, &mut (), mod_item::Event::SelectedToggle(s));
                Some((self.on_toggled_fn)(idx, s))
            }
        } 
    }
}

impl<'a, Message, Renderer> From<GroupWidget<'a, Message>> for iced_native::Element<'a, Message, Renderer> 
where
    Message: 'a,
    Renderer: iced_native::text::Renderer + 'static,
    Renderer::Theme: widget::text::StyleSheet
        + widget::checkbox::StyleSheet, {
    fn from(value: GroupWidget<'a, Message>) -> Self {
        iced_lazy::component(value)
    }

}
use itertools::Itertools;
impl<'a, Message> GroupWidget<'a, Message> 
    where Message: 'a,    
{
    pub fn new<F: Fn(usize, bool) -> Message + 'a>(name: String, infos: Vec<gmmlib_rs::ModInfo>, f: F) -> Self {
        let good_infos = infos.into_iter().enumerate().map(|(i, val)| mod_item::ModItem::new(val, move |it| Event::SelectedToggle(i, it))).collect_vec();
        Self { name, infos: good_infos, on_toggled_fn: Box::new(f) }
    }
}
