use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type VarID = String;
type VarVal = Option<i32>;
type VarCnt = Option<u32>;

#[derive(Debug, Clone)]
pub enum Sym {
    ConstInt(i32),
    ConstStr(String),
    VarInt((VarID, VarVal)),
}

#[derive(Debug)]
struct SymMap {
    symbols: HashMap<String, Sym>,
    parent: Option<Rc<RefCell<SymMap>>>,
    depth: u32,
    cnt: u32,
}

impl SymMap {
    fn new() -> Self {
        SymMap {
            symbols: HashMap::new(),
            parent: None,
            depth: 0,
            cnt: 1,
        }
    }

    fn insert(&mut self, ident: String, sym: Sym) {
        let sym_cnt: Sym = if let Sym::VarInt((var_id, var_val)) = sym {
            Sym::VarInt((format!("{}_{}", var_id, self.cnt), var_val))
        } else {
            sym
        };
        self.symbols.insert(ident, sym_cnt);
        self.cnt += 1;
    }

    fn search(&self, ident: String) -> Result<Sym, String> {
        match self.symbols.get(&ident) {
            None => match &self.parent {
                None => Err(format!("can't find symbol: {}!", ident)),
                Some(parent) => parent.borrow().search(ident),
            },
            Some(sym) => match sym {
                Sym::VarInt((id, var)) => {
                    let var_int = Sym::VarInt((format!("{}_{}", id, self.depth), *var));
                    Ok(var_int)
                }
                _ => Ok(sym.clone()),
            },
        }
    }
}

#[derive(Debug)]
pub struct SymTable {
    curr_map: Rc<RefCell<SymMap>>,
    tem_symbols: Vec<(VarID, bool, VarCnt)>,
    count: u32,
}

impl SymTable {
    pub fn new() -> Self {
        let rc_sym_map = Rc::new(RefCell::new(SymMap::new()));
        SymTable {
            curr_map: Rc::clone(&rc_sym_map),
            tem_symbols: Vec::new(),
            count: 0,
        }
    }
    pub fn get_cnt(&mut self) -> u32 {
        self.count
    }

    pub fn get_tsym(&mut self) -> String {
        let symbol: String = format!("%{}", self.count);
        self.tem_symbols
            .push((symbol.clone(), true, Some(self.count)));
        self.count += 1;
        symbol
    }

    pub fn find_tsym(&self, val: u32) -> Result<String, String> {
        for sym in self.tem_symbols.clone() {
            if let Some(sym_val) = sym.2 {
                if sym_val == val && sym.1 {
                    return Ok(sym.0);
                }
            }
        }
        Err(format!("can't find fitable temporary symbols: {}", val).to_string())
    }

    pub fn insert_sym(&mut self, ident: String, sym: Sym) {
        self.curr_map.borrow_mut().insert(ident, sym);
    }

    pub fn search_sym(&mut self, ident: String) -> Sym {
        match self.curr_map.borrow().search(ident) {
            Ok(sym) => sym,
            Err(e) => panic!("{}", e),
        }
    }

    pub fn add_sym_map(&mut self) {
        let new_sym_map = Rc::new(RefCell::new(SymMap::new()));
        new_sym_map.borrow_mut().parent = Some(Rc::clone(&self.curr_map));
        new_sym_map.borrow_mut().depth = self.curr_map.borrow().depth + 1;
        self.curr_map = Rc::clone(&new_sym_map);
    }

    pub fn delete_sym_map(&mut self) {
        let curr_map = Rc::clone(&self.curr_map);
        match &curr_map.borrow().parent {
            None => panic!("can't find parent symbol map!"),
            Some(parent) => {
                self.curr_map = Rc::clone(parent);
            }
        };
    }
}
