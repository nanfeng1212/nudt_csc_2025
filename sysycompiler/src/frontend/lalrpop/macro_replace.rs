use regex::Regex;
use once_cell::sync::Lazy;

// 使用 lazy_static 预编译正则表达式，避免重复编译
static STARTTIME_RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"starttime\s*\(\s*\)").expect("Invalid STARTTIME regex")
});

static STOPTIME_RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"stoptime\s*\(\s*\)").expect("Invalid STOPTIME regex")
});

/// 替换单个宏的通用函数
fn replace_macro(line: &str, regex: &Regex, replacement: &str) -> String {
    regex.replace_all(line, replacement).into_owned()
}

/// SysY 源码的预处理器
///
/// 主要功能是将源码中的：
///   `starttime()` → `_sysy_starttime(行号)`
///   `stoptime()` → `_sysy_stoptime(行号)`
pub fn replace_macros(src: &str) -> String {
    let mut result = String::with_capacity(src.len() * 2); // 预分配空间
    let mut lineno = 1;

    for line in src.lines() {
        // 按顺序替换两种宏
        let processed = replace_macro(line, &STARTTIME_RE, &format!("_sysy_starttime({lineno})"));
        let processed = replace_macro(&processed, &STOPTIME_RE, &format!("_sysy_stoptime({lineno})"));
        
        result.push_str(&processed);
        result.push('\n');
        lineno += 1;
    }
    
    result
}