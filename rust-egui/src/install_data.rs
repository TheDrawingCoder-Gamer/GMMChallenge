use std::collections::HashMap;
#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub struct InstallData {
    pub version: String,
    pub structure: Vec<String>,
}
#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub struct InstallDatas {
    pub mods: HashMap<String, InstallData>    
}

impl Default for InstallDatas {
    fn default() -> Self {
        Self {mods: HashMap::new()}
    }
}

impl InstallDatas {
    /// Merges preffereing that over this. 
    pub fn merge(&self, that: &InstallDatas) -> InstallDatas {
        let mut mods: HashMap<String, InstallData> = HashMap::new();
        self.mods.iter().for_each(|(k, v)| {
            mods.insert(k.clone(), v.clone());
        });
        that.mods.iter().for_each(|(k, v)| {
            mods.insert(k.clone(), v.clone());
        });
        InstallDatas { mods }
    }
}


