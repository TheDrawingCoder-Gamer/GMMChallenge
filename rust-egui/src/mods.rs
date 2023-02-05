use core::option::Option;
use curl::easy::Easy;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::sync::mpsc::Sender;
use zip::read::ZipArchive;
use crate::install_data::{self, *};
#[derive(Clone, Default, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct ModInfo {
    pub name: String,
    pub author: String,
    pub version: String,
    pub download_url: String,
    #[serde(default)]
    pub git_path: Option<String>,
    pub group: String,
    #[serde(default)]
    pub dependencies: Option<Vec<String>>,
    #[serde(default)]
    pub install_location: Option<String>,
    #[serde(default)]
    pub beta: bool,
}
pub fn mangle_name(name: &str) -> String {
    let ok_name = name.to_string();
    let chars: String = ok_name
        .chars()
        .map(|x| {
            if x.is_ascii_alphanumeric() {
                return x.to_ascii_lowercase() as char;
            } else {
                return '-' as char;
            }
        })
        .collect::<String>();
    return chars;
}
pub fn fetch_path() -> std::io::Result<String> {
    if Path::new("gtag_path.txt").exists() {
        let mut file = File::open("gtag_path.txt")?;
        let mut buf: Vec<u8> = Vec::new();
        file.read_to_end(&mut buf)?;
        let path = String::from_utf8(buf);
        return Ok(path.expect("expect utf8 encoding"));
    } else {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "No gtag path; run cligmm setup [path]",
        ));
    }
}

pub fn fetch_mods() -> Result<Vec<ModInfo>, String> {
    let url = "https://raw.githubusercontent.com/DeadlyKitten/MonkeModInfo/master/modinfo.json"
        .to_string();
    let mut easy = Easy::new();
    easy.url(&url).unwrap();
    easy.follow_location(true).unwrap();
    // direct string breaks this
    let mut buf: Vec<u8> = Vec::new();
    {
        let mut transfer = easy.transfer();
        transfer
            .write_function(|data| {
                buf.extend_from_slice(data);
                return Ok(data.len());
            })
            .unwrap();
        transfer.perform().unwrap();
    }
    let json_data = String::from_utf8(buf).unwrap();
    serde_json::from_str(&json_data).map_err(|it| it.to_string())
}

pub fn install_mod(
    infos: &Vec<ModInfo>,
    mod_info: &ModInfo,
    path: &str,
    tx: Option<Sender<String>>,
) -> std::io::Result<InstallData> {
    if let Some(deps) = &mod_info.dependencies {
        for dep in deps.iter() {
            let da_mod = (*infos)
                .iter()
                .find(|x| if *dep == x.name { true } else { false })
                .unwrap();
            install_mod(&infos, &da_mod, path, tx.clone())?;
        }
    }
    install_no_deps(mod_info, path, tx)
}

pub fn install_mods(
    infos: &Vec<ModInfo>,
    to_install: &Vec<ModInfo>,
    path: &str,
    tx: Option<Sender<String>>,
) -> std::io::Result<InstallDatas> {
    let mut deps: Vec<ModInfo> = to_install
        .iter()
        .flat_map(|it| it.dependencies.clone())
        .flatten()
        .map(|it| infos.iter().find(|ti| it == ti.name))
        .flatten()
        .map(|it| it.clone())
        .collect();
    deps.dedup();
    let mut install_data = InstallDatas::default();
    if !deps.is_empty() {
        install_data = install_mods(infos, &deps, path, tx.clone())?;
    }
    for m in to_install {
        install_data.mods.insert(m.version.clone(), install_no_deps(m, path, tx.clone())?);
    }
    Ok(install_data)
}
pub async fn install_mods_async(
    infos: &Vec<ModInfo>,
    to_install: &Vec<ModInfo>,
    path: &str,
    tx: Option<Sender<String>>,
) -> std::io::Result<InstallDatas> {
    install_mods(infos, to_install, path, tx)
}
pub async fn install_async(
    infos: &Vec<ModInfo>,
    mod_info: &ModInfo,
    path: &str,
    tx: Option<Sender<String>>,
) -> std::io::Result<InstallData> {
    install_mod(infos, mod_info, path, tx)
}
pub fn install_no_deps(
    mod_info: &ModInfo,
    path: &str,
    tx: Option<Sender<String>>,
) -> std::io::Result<install_data::InstallData> {
    if let Some(t) = tx {
        t.send(format!("Installing {}...", mod_info.name))
            .map_err(|it| std::io::Error::new(std::io::ErrorKind::Other, it))?;
    }
    let mut easy = Easy::new();
    easy.url(&mod_info.download_url).unwrap();
    easy.follow_location(true).unwrap();
    let mut buf = Vec::new();
    {
        let mut transfer = easy.transfer();
        transfer
            .write_function(|data| {
                buf.extend_from_slice(data);
                Ok(data.len())
            })
            .unwrap();
        transfer.perform().unwrap();
    }
    let reader = std::io::Cursor::new(buf);
    let mut archive = ZipArchive::new(reader).unwrap();
    let mut da_path: String = path.to_string();
    if let Some(install_location) = &mod_info.install_location {
        da_path = Path::new(&path)
            .join(install_location)
            .to_str()
            .unwrap()
            .to_string();
    }

    let info: Vec<String> = archive.file_names().map(|it| it.to_string()).collect();
    archive.extract(da_path).unwrap();
    return Ok(install_data::InstallData{structure: info, version: mod_info.version.clone()});
}