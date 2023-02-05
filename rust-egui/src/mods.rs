use core::option::Option;
use std::fs::File;
use std::io::Read;
use zip::read::ZipArchive;
use curl::easy::Easy;
use std::path::Path;

pub mod version;
#[derive(Clone, Default)]
pub struct ModInfo {
    pub name : String,
    pub author : String,
    pub version : String, 
    pub download_url : String,
    pub git_path : Option<String>,
    pub group : String,
    pub dependencies : Option<Vec<String>>,
    pub install_location: Option<String>,
    pub beta: bool
}
pub fn mangle_name(name:&str) -> String {
    let ok_name = name.to_string();
    let chars: String = ok_name.chars().map(|x| {
        if x.is_ascii_alphanumeric() {
            return x.to_ascii_lowercase() as char;
        } else {
            return '-' as char;
        }
    }).collect::<String>();
    return chars;
}
pub fn fetch_path() -> std::io::Result<String> {
    if Path::new("gtag_path.txt").exists() {
        let mut file = File::open("gtag_path.txt")?;
        let mut buf:Vec<u8> = Vec::new();
        file.read_to_end(&mut buf)?;
        let path = String::from_utf8(buf);
        return Ok(path.expect("expect utf8 encoding"));
    } else {
        return Err(std::io::Error::new(std::io::ErrorKind::NotFound, "No gtag path; run cligmm setup [path]"));

    }
}
pub fn make_modinfo(obj:&json::object::Object) -> Option<ModInfo> {
    let mut modinfo = ModInfo {
        name: String::from(""),
        author: String::from(""),
        version: String::from(""),
        download_url: String::from(""),
        git_path: None, 
        group: String::from(""),
        dependencies: None, 
        install_location: None, 
        beta: false
    };
    modinfo.name = obj.get("name").unwrap().as_str().unwrap().to_string();
    modinfo.author = obj.get("author").unwrap().as_str().unwrap().to_string();
    modinfo.version = obj.get("version").unwrap().as_str().unwrap().to_string();
    modinfo.download_url = obj.get("download_url").unwrap().as_str().unwrap().to_string();
    match obj.get("git_path") {
        Some(value) => {
            if let json::JsonValue::String(string) = value {
                modinfo.git_path = Some(string.to_string());
            }
        }
        None => {
            modinfo.git_path = None;
        }
    }
    modinfo.group = obj.get("group").unwrap().as_str().unwrap().to_string();
    match obj.get("dependencies") {
        Some(value) => {
            if let json::JsonValue::Array(vec) = value {
                let str_map: Vec<String> = vec.iter().filter_map(|x| { 
                    
                    if let Some(string) = x.as_str() {
                        
                        return Some(string.to_string());
                    } else {
                        None
                        // return String::from("");
                    }
                }).collect();
                
                modinfo.dependencies = Some(str_map);
            }
        }
        None => {
            modinfo.dependencies = None;
        }
    }
    match obj.get("install_location") {
        Some(value) => {
            modinfo.install_location = Some(value.as_str().unwrap().to_string());
        }
        None => {
            modinfo.install_location = None;
        }
    }
    match obj.get("beta") {
        Some(value) => {
            if let json::JsonValue::Boolean(boolean) = value {
                modinfo.beta = *boolean;
            }
        }
        None => {
            modinfo.beta = false;
        }
    }
    Some(modinfo)
    
}

pub fn fetch_json() -> json::JsonValue {
    let url = "https://raw.githubusercontent.com/DeadlyKitten/MonkeModInfo/master/modinfo.json".to_string();
    let mut easy = Easy::new();
    easy.url(&url).unwrap();
    easy.follow_location(true).unwrap();
    // direct string breaks this
    let mut buf:Vec<u8> = Vec::new(); 
    {
        let mut transfer = easy.transfer();
        transfer.write_function(|data| {
            buf.extend_from_slice(data);
            return Ok(data.len());
        }).unwrap();
        transfer.perform().unwrap();
    }
    let json_data = String::from_utf8(buf).unwrap();
    return json::parse(&json_data).unwrap_or(json::JsonValue::Null);
}

pub fn install_mod(infos:&Vec<ModInfo>, mod_info: &ModInfo, path: &str) -> std::io::Result<()> {
    
    if let Some(deps) = &mod_info.dependencies {
        for dep in deps.iter() {
            let da_mod = (*infos).iter().find(|x| {
                if *dep == x.name {
                    true
                } else {
                    false
                }
            }).unwrap();
            install_mod(&infos, &da_mod, path)?;            
        }
    }
    install_no_deps(mod_info, path)
}

pub async fn install_async(infos: &Vec<ModInfo>, mod_info: &ModInfo, path: &str) -> std::io::Result<()> {
    install_mod(infos, mod_info, path)
}
pub fn install_no_deps(mod_info: &ModInfo, path: &str) -> std::io::Result<()> {
    println!("Installing {}...", mod_info.name);
    let mut easy = Easy::new();
    easy.url(&mod_info.download_url).unwrap();
    easy.follow_location(true).unwrap();
    let mut buf = Vec::new();
    {
        let mut transfer = easy.transfer();
        transfer.write_function(|data| {
            buf.extend_from_slice(data);
            Ok(data.len())
        }).unwrap();
        transfer.perform().unwrap();
    }
    let reader = std::io::Cursor::new(buf);
    let mut archive = ZipArchive::new(reader).unwrap();
    let mut da_path: String = path.to_string(); 
    if let Some(install_location) = &mod_info.install_location {
        da_path = Path::new(&path).join(install_location).to_str().unwrap().to_string();
    }
    archive.extract(da_path).unwrap();
    return Ok(());
}

