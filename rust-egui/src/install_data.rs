use std::collections::HashMap;
#[derive(serde::Serialize, serde::Deserialize)]
pub struct InstallData {
    pub version: String,
    pub structure: Vec<String>,
}
#[derive(serde::Serialize, serde::Deserialize)]
pub struct InstallDatas {
    pub mods: HashMap<String, InstallData>    
}

impl Default for InstallDatas {
    fn default() -> Self {
        Self {mods: HashMap::new()}
    }
}


