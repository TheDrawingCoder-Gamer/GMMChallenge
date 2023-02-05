use std::collections::HashMap;


pub fn mangle_name(name: &str) -> String {
    return name.to_lowercase().trim().replace(" ", "_");
}
pub fn full_mangle(name: &str) -> String {
    let mangled = mangle_name(name);
    mangled.chars().map(|c| {
        if !c.is_ascii_alphanumeric() {
            '_'
        } else {
            c
        }
    }).collect()
}

pub enum UpdateStatus {
    UpToDate,
    Outdated,
}
pub struct InstallData {
    pub structure: Vec<String>,
    pub version: String,
}

impl InstallData {
    pub fn is_latest(&self, version: &str) -> bool {
        self.version == version
    }
    pub fn status(&self, version: &str) -> UpdateStatus {
        if self.is_latest(version) {
            UpdateStatus::UpToDate
        } else {
            UpdateStatus::Outdated
        }
    }
}
impl From<InstallData> for json::JsonValue {
    fn from(value: InstallData) -> Self {
        json::object! {
            version: value.version,
            structure: Into::<json::JsonValue>::into(value.structure)
        }
    }
}
impl TryFrom<json::JsonValue> for InstallData {
    type Error = String;
    fn try_from(value: json::JsonValue) -> Result<Self, Self::Error> {
        if let json::JsonValue::Object(obj) = value {
            if let json::JsonValue::Array(structure) = obj.get("structure").ok_or("No such field 'structure'")? {
                let s: Vec<String> = structure.iter().map(|o| { o.as_str().map(|s| { s.to_string() }) }).flatten().collect();
                let version: String = obj.get("version").ok_or("No such field 'version'")?.as_str().ok_or("Not a string")?.to_string();

                Ok(InstallData { structure: s, version })
            } else {
                Err("Structure is not an array".to_string())
            }
        } else {
            Err("Not an object".to_string())
        }
    }
}
pub struct VersionData {
    mods: HashMap<String, InstallData>
}

impl VersionData {
    pub fn new() -> Self {
        VersionData { mods: HashMap::new() }
    }
    pub fn add_mod(&mut self, name: &str, data: InstallData) -> () {
        self.mods.insert(full_mangle(name), data);
    }
    pub fn remove_mod(&mut self, name: &str) -> Option<InstallData> {
        self.mods.remove(&full_mangle(name))
    }
    pub fn get_mod(&self, name: &str) -> Option<&InstallData> {
        self.mods.get(&full_mangle(name))
    }
    pub fn status(&self, name: &str, version: &str) -> Option<UpdateStatus> {
        self.get_mod(name).map(|o| { o.status(version) })
    }
}
impl From<VersionData> for json::JsonValue {
    fn from(value: VersionData) -> Self {
        value.mods.into()
    }
}

impl TryFrom<json::JsonValue> for VersionData {
    type Error = String;
    fn try_from(value: json::JsonValue) -> Result<Self, Self::Error> {
        if value.is_object() {
            let mods: HashMap<String, InstallData> = value.entries().flat_map(|(l, r)| {
                TryInto::<InstallData>::try_into(r.clone()).map(|rr| { (l.to_string() , rr)})
            }).collect();
            Ok(VersionData {mods})
        } else {
            Err("Not an object".to_string())
        }
    }
}

