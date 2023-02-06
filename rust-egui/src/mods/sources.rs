
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum SourceItem {
    Group(String),
    Mods(String),
    AssetMods(String),
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Source {
    pub item: SourceItem,
}

pub enum Condition {
    Or(Box<Condition>, Box<Condition>),
    And(Box<Condition>, Box<Condition>),
    Not(Box<Condition>),
    Ident(String),
    Bool(bool),
}
#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Group {
    pub rank: u32,
    pub name: String,
}
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Sources(Vec<Source>);

use crate::mods;
use directories_next::*;
impl Sources {
    fn get_file() -> std::io::Result<std::path::PathBuf> {
        if let Some(path) = ProjectDirs::from("net", "BulbyVR", "Rusty Gorilla Mod Manager") {
            Ok(path.data_dir().join("sources.ron"))
        } else {
            Err(std::io::Error::new(std::io::ErrorKind::NotFound, "net.BulbyVR.Rusty Gorilla Mod Manager")) 
        }
    }
    pub fn save(&self) -> std::io::Result<()> {
        let path = Sources::get_file()?;
        let res = ron::ser::to_string_pretty(self, ron::ser::PrettyConfig::new()).map_err(|it| std::io::Error::new(std::io::ErrorKind::Other, it))?;
        std::fs::write(path, &res)

    }
    pub fn read() -> std::io::Result<Self> {
        let file = Sources::get_file()?;
        if file.exists() {
            let data = std::fs::read(&file)?;
            let json: Sources = ron::de::from_bytes(&data).map_err(|it| std::io::Error::new(std::io::ErrorKind::Other, it))?; 
            Ok(json)
        } else {
            Err(std::io::Error::new(std::io::ErrorKind::NotFound, file.to_string_lossy()))
        }
    }
    pub fn ensure_existance() -> std::io::Result<()> {
        let path = Sources::get_file()?;
        if !path.exists() {
            Sources::default().save()?;       
        }
        Ok(())
    }
    /// panics when not getting that good shit LOL
    pub fn render(&self) -> RenderedSources {
        let mut groups: Vec<Group> = Vec::new();
        let mut infos: Vec<mods::ModInfo> = Vec::new();
        self.0.iter()
            .for_each(|it| {
                match &it.item {
                    SourceItem::Mods(item) => {
                        let mut mods = mods::fetch_mods(item).unwrap();
                        infos.append(&mut mods);
                    }
                    SourceItem::AssetMods(_item) => unimplemented!(),
                    SourceItem::Group(group) => {
                        let data = mods::download(&group).unwrap();
                        let mut gs: Vec<Group> = serde_json::from_slice(&data).unwrap();
                        groups.append(&mut gs);

                    }
                }
            });
        // unstable - only sorting to dedup
        groups.sort_unstable_by(|l, r| l.name.cmp(&r.name));
        groups.dedup_by(|l, r| l.name == r.name);
        infos.sort_unstable_by(|l, r| l.name.cmp(&r.name));
        infos.dedup_by(|l, r| l.name == r.name);
        RenderedSources { infos, groups }
    }
}

#[derive(Clone)]
pub struct RenderedSources {
    pub infos: Vec<mods::ModInfo>,
    pub groups: Vec<Group>,
}

impl Source {
    pub fn new(item: SourceItem) -> Source {
        Self { item, }
    }

}

// A default in the sense of PLEASE SAVE THIS 
impl Default for Sources {
    fn default() -> Self {
        Self(vec![Source::new(SourceItem::Group("https://raw.githubusercontent.com/DeadlyKitten/MonkeModInfo/master/groupinfo.json".to_string())),
             Source::new(SourceItem::Mods("https://raw.githubusercontent.com/DeadlyKitten/MonkeModInfo/master/modinfo.json".to_string()))])
    }
}




