use std::collections::HashMap;

pub struct StringTab {
    pub str_tbl : HashMap<u64, String>,
    str_idx : HashMap<String, u64>,
    count : u64,
}

impl StringTab {
    pub fn new() -> StringTab {
        let mut str_init = HashMap::new();
        str_init.insert(0, "<ERROR>".to_string());
        StringTab { str_tbl : str_init, str_idx : HashMap::new(), count : 1 }
    }

    pub fn add_string<'a>(&mut self, s : &'a str) -> u64 {
        match self.str_idx.get(s) {
            Some(i) => *i,
            None => {
                let idx = self.count;
                self.count += 1;
                self.str_tbl.insert(idx, s.to_string());
                self.str_idx.insert(s.to_string(), idx);
                idx
            },
        }
    }
}
