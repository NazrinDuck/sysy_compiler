use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

pub trait DumpIR {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String;
}

pub trait ParseSym {
    fn parse_sym(&self, sym_table: &mut SymTable);
}

pub trait DumpVal {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32;
}

type VarID = String;

#[derive(Debug, Clone)]
enum Sym {
    ConstInt(i32),
    ConstStr(String),
    VarInt((VarID, Option<i32>)),
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
    tem_symbols: Vec<(String, bool, Option<u32>)>,
    count: u32,
    block_cnt: u32,
}

impl SymTable {
    pub fn new() -> Self {
        let rc_sym_map = Rc::new(RefCell::new(SymMap::new()));
        SymTable {
            curr_map: Rc::clone(&rc_sym_map),
            tem_symbols: Vec::new(),
            count: 0,
            block_cnt: 0,
        }
    }

    fn get_tsym(&mut self) -> String {
        let symbol: String = format!("%{}", self.count);
        self.tem_symbols
            .push((symbol.clone(), true, Some(self.count)));
        self.count += 1;
        symbol
    }

    fn find_tsym(&self, val: u32) -> Result<String, String> {
        for sym in self.tem_symbols.clone() {
            if let Some(sym_val) = sym.2 {
                if sym_val == val && sym.1 {
                    return Ok(sym.0);
                }
            }
        }
        Err(format!("can't find fitable temporary symbols: {}", val).to_string())
    }

    fn insert_sym(&mut self, ident: String, sym: Sym) {
        self.curr_map.borrow_mut().insert(ident, sym);
    }
    fn search_sym(&mut self, ident: String) -> Sym {
        match self.curr_map.borrow().search(ident) {
            Ok(sym) => sym,
            Err(e) => panic!("{}", e),
        }
    }

    fn add_sym_map(&mut self) {
        let new_sym_map = Rc::new(RefCell::new(SymMap::new()));
        new_sym_map.borrow_mut().parent = Some(Rc::clone(&self.curr_map));
        new_sym_map.borrow_mut().depth = self.curr_map.borrow().depth + 1;
        self.curr_map = Rc::clone(&new_sym_map);
    }
    fn delete_sym_map(&mut self) {
        let curr_map = Rc::clone(&self.curr_map);
        match &curr_map.borrow().parent {
            None => panic!("can't find parent symbol map!"),
            Some(parent) => {
                self.curr_map = Rc::clone(parent);
            }
        };
    }
    fn get_block_name(&mut self) -> String {
        let name = format!("BR{}", self.block_cnt);
        self.block_cnt += 1;
        name
    }
}

#[derive(Debug)]
struct BaseBlock {
    name: String,
    is_end: bool,
    ir: String,
    next: Vec<String>,
}
// ir bond base_block is a good idea ithk

impl BaseBlock {
    fn new(name: String) -> Self {
        BaseBlock {
            name,
            is_end: false,
            ir: String::new(),
            next: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct BlockGraph {
    map: HashMap<String, BaseBlock>,
    entry: String,
    curr_map: String,
}

impl BlockGraph {
    pub fn new() -> Self {
        BlockGraph {
            map: HashMap::new(),
            entry: String::new(),
            curr_map: String::new(),
        }
    }

    fn find_mut_block(&mut self, name: &String) -> &mut BaseBlock {
        match self.map.get_mut(name) {
            Some(base_block) => base_block,
            None => panic!("the map is empty!"),
        }
    }

    fn find_block(&self, name: &String) -> &BaseBlock {
        match self.map.get(name) {
            Some(base_block) => base_block,
            None => panic!("the map is empty!"),
        }
    }

    fn curr_block(&mut self) -> &mut BaseBlock {
        let name: String = self.curr_map.clone();
        self.find_mut_block(&name)
    }

    fn insert(&mut self, name: String) {
        let new_base_block = BaseBlock::new(name.clone());

        /*
                if let Some(base_block) = self.map.get_mut(&self.curr_map) {
                    base_block.next.push(name.clone());
                }
        */

        self.curr_map = name.clone();
        self.map.insert(name, new_base_block);
    }

    fn set_entry(&mut self, enrty: String) {
        self.insert(enrty.clone());
        self.entry = enrty;
    }
    fn end(&mut self, next: Option<String>) {
        self.curr_block().is_end = true;

        if let Some(next_name) = next {
            self.insert(next_name);
        }
    }

    fn is_end(&self) -> bool {
        self.map.get(&self.curr_map).unwrap().is_end
    }

    fn insert_next(&mut self, next: String) {
        self.curr_block().next.push(next);
    }

    fn insert_ir(&mut self, ir: &String) {
        if !self.curr_block().is_end {
            self.curr_block().ir += ir;
        }
    }

    pub fn generate_ir(&self) -> String {
        let mut queue: VecDeque<String> = VecDeque::new();
        queue.push_back(self.entry.clone());
        let mut set: HashSet<String> = HashSet::new();
        let mut ir: String = String::new();
        set.insert(self.entry.clone());

        while !queue.is_empty() {
            let name = queue.pop_front().unwrap();
            ir += &format!("%{}:\n{}", name, self.find_block(&name).ir.clone());
            for ele in self.map.get(&name).unwrap().next.clone() {
                if !set.contains(&ele) {
                    set.insert(ele.clone());
                    queue.push_back(ele);
                }
            }
        }
        ir
    }
}

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

impl DumpIR for CompUnit {
    /// Start function
    /// All start here
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        self.func_def.dump_ir(sym_table, block_graph)
    }
}

#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

impl DumpIR for Decl {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        match self {
            Decl::ConstDecl(const_decl) => {
                const_decl.parse_sym(sym_table);
                String::new()
            }
            Decl::VarDecl(var_decl) => {
                var_decl.parse_sym(sym_table);
                let ir: String = var_decl.dump_ir(sym_table, block_graph);

                block_graph.curr_block().ir += &ir;
                ir
            }
        }
    }
}

#[derive(Debug)]
pub struct ConstDecl {
    pub b_type: BType,
    pub const_defs: Vec<ConstDef>,
}

impl ParseSym for ConstDecl {
    fn parse_sym(&self, sym_table: &mut SymTable) {
        for const_def in &self.const_defs {
            match self.b_type {
                BType::Int => {
                    let val = const_def.const_init_val.dump_val(sym_table);
                    sym_table.insert_sym(const_def.ident.clone(), Sym::ConstInt(val));
                }
            };
        }
    }
}

#[derive(Debug)]
pub enum BType {
    Int,
}

#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub const_init_val: ConstInitVal,
}

impl DumpVal for ConstInitVal {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32 {
        self.const_exp.dump_val(sym_table)
    }
}

#[derive(Debug)]
pub struct ConstInitVal {
    pub const_exp: ConstExp,
}

#[derive(Debug)]
pub struct VarDecl {
    pub b_type: BType,
    pub var_defs: Vec<VarDef>,
}

impl ParseSym for VarDecl {
    fn parse_sym(&self, sym_table: &mut SymTable) {
        for var_def in &self.var_defs {
            let ident: String = format!("@{}", var_def.ident);
            match self.b_type {
                BType::Int => match &var_def.init_val {
                    Some(init_val) => {
                        let val = init_val.dump_val(sym_table);
                        sym_table
                            .insert_sym(var_def.ident.clone(), Sym::VarInt((ident, Some(val))));
                    }
                    None => {
                        sym_table.insert_sym(var_def.ident.clone(), Sym::VarInt((ident, None)));
                    }
                },
            };
        }
    }
}

impl DumpIR for VarDecl {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        let sym_type: String = match self.b_type {
            BType::Int => "i32".to_string(),
        };
        let mut ir: String = String::new();
        for var_def in &self.var_defs {
            let ident = match sym_table.search_sym(var_def.ident.clone()) {
                Sym::VarInt((ident, _)) => ident.clone(),
                _ => panic!("symbol has already defined!"),
            };
            ir += &format!("\t{} = alloc {}\n", ident, sym_type);
            if let Some(init_val) = &var_def.init_val {
                let pre_cnt = sym_table.count;
                let init_val_ir = init_val.dump_ir(sym_table, block_graph);
                let var_cnt = sym_table.count;

                if pre_cnt == var_cnt {
                    ir += &format!("\tstore {}, {}\n", init_val_ir, ident);
                } else {
                    ir += &format!(
                        "{}\tstore {}, {}\n",
                        init_val_ir,
                        sym_table.find_tsym(var_cnt - 1).unwrap(),
                        ident
                    );
                }
            }
        }
        ir
    }
}

#[derive(Debug)]
pub struct VarDef {
    pub ident: String,
    pub init_val: Option<InitVal>,
}

#[derive(Debug)]
pub struct InitVal {
    pub exp: Exp,
}
impl DumpIR for InitVal {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        self.exp.dump_ir(sym_table, block_graph)
    }
}

impl DumpVal for InitVal {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32 {
        self.exp.dump_val(sym_table)
    }
}

#[derive(Debug)]
pub struct ConstExp {
    pub exp: Exp,
}

impl DumpVal for ConstExp {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32 {
        self.exp.dump_val(sym_table)
    }
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

impl DumpIR for FuncDef {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        block_graph.set_entry(String::from("entry"));
        let ir = self.block.dump_ir(sym_table, block_graph);
        format!(
            "fun @{ident}(): {func_type} {{\n{block}}}",
            ident = self.ident,
            func_type = self.func_type.dump_ir(sym_table, block_graph),
            //block = ir,
            block = block_graph.generate_ir(),
        )
    }
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

impl DumpIR for FuncType {
    fn dump_ir(&self, _: &mut SymTable, _: &mut BlockGraph) -> String {
        match self {
            FuncType::Int => "i32".to_string(),
        }
    }
}

#[derive(Debug)]
pub struct LVal {
    pub ident: String,
}

#[derive(Debug)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    LVal(LVal),
    Number(i32),
}

impl DumpIR for PrimaryExp {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        match self {
            PrimaryExp::Exp(exp) => exp.dump_ir(sym_table, block_graph),
            PrimaryExp::LVal(lval) => {
                let sym = sym_table.search_sym(lval.ident.clone());
                match sym {
                    Sym::ConstInt(val) => val.to_string(),
                    Sym::ConstStr(str) => str.clone(),
                    Sym::VarInt((ident, _)) => {
                        format!("\t{} = load {}\n", sym_table.get_tsym(), ident.clone())
                    }
                }
            }
            PrimaryExp::Number(num) => num.to_string(),
        }
    }
}

impl DumpVal for PrimaryExp {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32 {
        match self {
            PrimaryExp::Exp(exp) => exp.dump_val(sym_table),
            PrimaryExp::LVal(lval) => match sym_table.search_sym(lval.ident.clone()) {
                Sym::ConstInt(val) => val,
                Sym::ConstStr(_) => 0,
                Sym::VarInt((_, val)) => val.unwrap_or(0),
            },
            PrimaryExp::Number(num) => *num,
        }
    }
}

#[derive(Debug)]
pub struct Exp {
    pub lor_exp: LOrExp,
}

impl DumpIR for Exp {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        self.lor_exp.dump_ir(sym_table, block_graph)
    }
}

impl DumpVal for Exp {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32 {
        self.lor_exp.dump_val(sym_table)
    }
}

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(Box<PrimaryExp>),
    Exp((UnaryOp, Box<UnaryExp>)),
}

impl DumpIR for UnaryExp {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        match self {
            Self::PrimaryExp(primary_exp) => primary_exp.dump_ir(sym_table, block_graph),
            Self::Exp((unary_op, unary_exp)) => {
                let pre_cnt = sym_table.count;
                let unary_exp_ir: String = unary_exp.dump_ir(sym_table, block_graph);
                let unary_cnt = sym_table.count;

                let mut ir: String;

                if unary_cnt == pre_cnt {
                    match unary_op {
                        UnaryOp::Positive => {
                            ir = unary_exp_ir.to_string();
                        }
                        UnaryOp::Negative => {
                            ir = format!("\t{} = sub 0, {}\n", sym_table.get_tsym(), unary_exp_ir);
                        }
                        UnaryOp::Not => {
                            ir = format!("\t{} = eq {}, 0\n", sym_table.get_tsym(), unary_exp_ir);
                        }
                    }
                } else {
                    ir = unary_exp_ir.to_string();
                    match unary_op {
                        UnaryOp::Positive => (),
                        UnaryOp::Negative => {
                            ir += &format!(
                                "\t{} = sub 0, {}\n",
                                sym_table.get_tsym(),
                                sym_table.find_tsym(unary_cnt - 1).unwrap()
                            );
                        }
                        UnaryOp::Not => {
                            ir += &format!(
                                "\t{} = eq {}, 0\n",
                                sym_table.get_tsym(),
                                sym_table.find_tsym(unary_cnt - 1).unwrap()
                            );
                        }
                    }
                }

                ir
            }
        }
    }
}

impl DumpVal for UnaryExp {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32 {
        match self {
            Self::PrimaryExp(primary_exp) => primary_exp.dump_val(sym_table),
            Self::Exp((unary_op, unary_exp)) => {
                let val = unary_exp.dump_val(sym_table);
                match unary_op {
                    UnaryOp::Positive => val,
                    UnaryOp::Negative => -val,
                    UnaryOp::Not => !val,
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum MulExp {
    UnaryExp(Box<UnaryExp>),
    Exp((Box<MulExp>, MulOp, Box<UnaryExp>)),
}

impl DumpIR for MulExp {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        match self {
            Self::UnaryExp(unary_exp) => unary_exp.dump_ir(sym_table, block_graph),
            Self::Exp((mul_exp, mul_op, unary_exp)) => {
                let pre_cnt = sym_table.count;
                let mul_exp_ir: String = mul_exp.dump_ir(sym_table, block_graph);
                let lhs_cnt = sym_table.count;
                let unary_exp_ir: String = unary_exp.dump_ir(sym_table, block_graph);
                let rhs_cnt = sym_table.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = mul_exp_ir;
                } else {
                    lhs_sym = sym_table.find_tsym(lhs_cnt - 1).unwrap();
                    ir += &mul_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = unary_exp_ir;
                } else {
                    rhs_sym = sym_table.find_tsym(rhs_cnt - 1).unwrap();
                    ir += &unary_exp_ir.to_string();
                }

                match mul_op {
                    MulOp::Mul => {
                        ir += &format!(
                            "\t{} = mul {}, {}\n",
                            sym_table.get_tsym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    MulOp::Div => {
                        ir += &format!(
                            "\t{} = div {}, {}\n",
                            sym_table.get_tsym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    MulOp::Mod => {
                        ir += &format!(
                            "\t{} = mod {}, {}\n",
                            sym_table.get_tsym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                };

                ir
            }
        }
    }
}

impl DumpVal for MulExp {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32 {
        match self {
            Self::UnaryExp(unary_exp) => unary_exp.dump_val(sym_table),
            Self::Exp((mul_exp, mul_op, unary_exp)) => {
                let lval = mul_exp.dump_val(sym_table);
                let rval = unary_exp.dump_val(sym_table);
                match mul_op {
                    MulOp::Mul => lval * rval,
                    MulOp::Div => lval / rval,
                    MulOp::Mod => lval % rval,
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum AddExp {
    MulExp(Box<MulExp>),
    Exp((Box<AddExp>, AddOp, Box<MulExp>)),
}

impl DumpIR for AddExp {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        match self {
            Self::MulExp(mul_exp) => (*mul_exp).dump_ir(sym_table, block_graph),
            Self::Exp((add_exp, add_op, mul_exp)) => {
                let pre_cnt = sym_table.count;
                let add_exp_ir: String = add_exp.dump_ir(sym_table, block_graph);
                let lhs_cnt = sym_table.count;
                let mul_exp_ir: String = mul_exp.dump_ir(sym_table, block_graph);
                let rhs_cnt = sym_table.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = add_exp_ir;
                } else {
                    lhs_sym = sym_table.find_tsym(lhs_cnt - 1).unwrap();
                    ir += &add_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = mul_exp_ir;
                } else {
                    rhs_sym = sym_table.find_tsym(rhs_cnt - 1).unwrap();
                    ir += &mul_exp_ir.to_string();
                }

                match add_op {
                    AddOp::Add => {
                        ir += &format!(
                            "\t{} = add {}, {}\n",
                            sym_table.get_tsym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    AddOp::Sub => {
                        ir += &format!(
                            "\t{} = sub {}, {}\n",
                            sym_table.get_tsym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                };
                ir
            }
        }
    }
}

impl DumpVal for AddExp {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32 {
        match self {
            Self::MulExp(mul_exp) => mul_exp.dump_val(sym_table),
            Self::Exp((add_exp, add_op, mul_exp)) => {
                let lval = add_exp.dump_val(sym_table);
                let rval = mul_exp.dump_val(sym_table);
                match add_op {
                    AddOp::Add => lval + rval,
                    AddOp::Sub => lval - rval,
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum RelExp {
    AddExp(AddExp),
    Exp((Box<RelExp>, RelOp, AddExp)),
}

impl DumpIR for RelExp {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        match self {
            Self::AddExp(add_exp) => add_exp.dump_ir(sym_table, block_graph),
            Self::Exp((rel_exp, rel_op, add_exp)) => {
                let pre_cnt = sym_table.count;
                let rel_exp_ir: String = rel_exp.dump_ir(sym_table, block_graph);
                let lhs_cnt = sym_table.count;
                let add_exp_ir: String = add_exp.dump_ir(sym_table, block_graph);
                let rhs_cnt = sym_table.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = rel_exp_ir;
                } else {
                    lhs_sym = sym_table.find_tsym(lhs_cnt - 1).unwrap();
                    ir += &rel_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = add_exp_ir;
                } else {
                    rhs_sym = sym_table.find_tsym(rhs_cnt - 1).unwrap();
                    ir += &add_exp_ir.to_string();
                }

                match rel_op {
                    RelOp::Gt => {
                        ir +=
                            &format!("\t{} = gt {}, {}\n", sym_table.get_tsym(), lhs_sym, rhs_sym);
                    }
                    RelOp::Lt => {
                        ir +=
                            &format!("\t{} = lt {}, {}\n", sym_table.get_tsym(), lhs_sym, rhs_sym);
                    }
                    RelOp::Ge => {
                        ir +=
                            &format!("\t{} = ge {}, {}\n", sym_table.get_tsym(), lhs_sym, rhs_sym);
                    }
                    RelOp::Le => {
                        ir +=
                            &format!("\t{} = le {}, {}\n", sym_table.get_tsym(), lhs_sym, rhs_sym);
                    }
                };
                ir
            }
        }
    }
}

impl DumpVal for RelExp {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32 {
        match self {
            Self::AddExp(add_exp) => add_exp.dump_val(sym_table),
            Self::Exp((rel_exp, rel_op, add_exp)) => {
                let lval = rel_exp.dump_val(sym_table);
                let rval = add_exp.dump_val(sym_table);
                match rel_op {
                    RelOp::Gt => (lval > rval) as i32,
                    RelOp::Lt => (lval < rval) as i32,
                    RelOp::Ge => (lval >= rval) as i32,
                    RelOp::Le => (lval <= rval) as i32,
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum EqExp {
    RelExp(RelExp),
    Exp((Box<EqExp>, EqOp, RelExp)),
}

impl DumpIR for EqExp {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        match self {
            Self::RelExp(rel_exp) => rel_exp.dump_ir(sym_table, block_graph),
            Self::Exp((eq_exp, eq_op, rel_exp)) => {
                let pre_cnt = sym_table.count;
                let eq_exp_ir: String = eq_exp.dump_ir(sym_table, block_graph);
                let lhs_cnt = sym_table.count;
                let rel_exp_ir: String = rel_exp.dump_ir(sym_table, block_graph);
                let rhs_cnt = sym_table.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = eq_exp_ir;
                } else {
                    lhs_sym = sym_table.find_tsym(lhs_cnt - 1).unwrap();
                    ir += &eq_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = rel_exp_ir;
                } else {
                    rhs_sym = sym_table.find_tsym(rhs_cnt - 1).unwrap();
                    ir += &rel_exp_ir.to_string();
                }

                match eq_op {
                    EqOp::Eq => {
                        ir +=
                            &format!("\t{} = eq {}, {}\n", sym_table.get_tsym(), lhs_sym, rhs_sym);
                    }
                    EqOp::NotEq => {
                        ir +=
                            &format!("\t{} = ne {}, {}\n", sym_table.get_tsym(), lhs_sym, rhs_sym);
                    }
                };
                ir
            }
        }
    }
}

impl DumpVal for EqExp {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32 {
        match self {
            Self::RelExp(rel_exp) => rel_exp.dump_val(sym_table),
            Self::Exp((eq_exp, eq_op, rel_exp)) => {
                let lval = eq_exp.dump_val(sym_table);
                let rval = rel_exp.dump_val(sym_table);
                match eq_op {
                    EqOp::Eq => (lval == rval) as i32,
                    EqOp::NotEq => (lval != rval) as i32,
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum LAndExp {
    EqExp(EqExp),
    Exp((Box<LAndExp>, EqExp)),
}

impl DumpIR for LAndExp {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        match self {
            Self::EqExp(eq_exp) => eq_exp.dump_ir(sym_table, block_graph),
            Self::Exp((land_exp, eq_exp)) => {
                let pre_cnt = sym_table.count;
                let land_exp_ir: String = land_exp.dump_ir(sym_table, block_graph);
                let lhs_cnt = sym_table.count;
                let eq_exp_ir: String = eq_exp.dump_ir(sym_table, block_graph);
                let rhs_cnt = sym_table.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = land_exp_ir;
                } else {
                    lhs_sym = sym_table.find_tsym(lhs_cnt - 1).unwrap();
                    ir += &land_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = eq_exp_ir;
                } else {
                    rhs_sym = sym_table.find_tsym(rhs_cnt - 1).unwrap();
                    ir += &eq_exp_ir.to_string();
                }

                let tem_res = sym_table.get_tsym();
                ir += &format!(
                    "\t{tem_res} = mul {}, {}\n\t{} = ne {tem_res}, 0\n",
                    lhs_sym,
                    rhs_sym,
                    sym_table.get_tsym(),
                );

                ir
            }
        }
    }
}

impl DumpVal for LAndExp {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32 {
        match self {
            Self::EqExp(eq_exp) => eq_exp.dump_val(sym_table),
            Self::Exp((land_exp, eq_exp)) => {
                (land_exp.dump_val(sym_table) != 0 && eq_exp.dump_val(sym_table) != 0) as i32
            }
        }
    }
}

#[derive(Debug)]
pub enum LOrExp {
    LAndExp(LAndExp),
    Exp((Box<LOrExp>, LAndExp)),
}

impl DumpIR for LOrExp {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        match self {
            Self::LAndExp(land_exp) => land_exp.dump_ir(sym_table, block_graph),
            Self::Exp((lor_exp, land_exp)) => {
                let pre_cnt = sym_table.count;
                let lor_exp_ir: String = lor_exp.dump_ir(sym_table, block_graph);
                let lhs_cnt = sym_table.count;
                let land_exp_ir: String = land_exp.dump_ir(sym_table, block_graph);
                let rhs_cnt = sym_table.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = lor_exp_ir;
                } else {
                    lhs_sym = sym_table.find_tsym(lhs_cnt - 1).unwrap();
                    ir += &lor_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = land_exp_ir;
                } else {
                    rhs_sym = sym_table.find_tsym(rhs_cnt - 1).unwrap();
                    ir += &land_exp_ir.to_string();
                }

                let tem_res = sym_table.get_tsym();
                ir += &format!(
                    "\t{tem_res} = or {}, {}\n\t{} = ne {tem_res}, 0\n",
                    lhs_sym,
                    rhs_sym,
                    sym_table.get_tsym(),
                );

                ir
            }
        }
    }
}

impl DumpVal for LOrExp {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32 {
        match self {
            Self::LAndExp(land_exp) => land_exp.dump_val(sym_table),
            Self::Exp((lor_exp, land_exp)) => {
                (lor_exp.dump_val(sym_table) != 0 || land_exp.dump_val(sym_table) != 0) as i32
            }
        }
    }
}

#[derive(Debug)]
pub enum RelOp {
    Gt,
    Lt,
    Ge,
    Le,
}

#[derive(Debug)]
pub enum EqOp {
    Eq,
    NotEq,
}

#[derive(Debug)]
pub enum UnaryOp {
    Positive,
    Negative,
    Not,
}

#[derive(Debug)]
pub enum AddOp {
    Add,
    Sub,
}

#[derive(Debug)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

impl DumpIR for Block {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        let mut ir: String = String::new();
        sym_table.add_sym_map();
        for block_item in &self.block_items {
            ir += &block_item.dump_ir(sym_table, block_graph);
        }
        sym_table.delete_sym_map();
        ir
    }
}

#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

impl DumpIR for BlockItem {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        //block_graph.push(self);

        let mut ir: String = String::new();
        match self {
            BlockItem::Decl(decl) => {
                ir += &decl.dump_ir(sym_table, block_graph);
            }
            BlockItem::Stmt(stmt) => {
                ir += &stmt.dump_ir(sym_table, block_graph);
            }
        };
        ir
    }
}

#[derive(Debug)]
pub enum Stmt {
    SetLVal(LVal, Exp),
    Exp(Option<Exp>),
    Block(Box<Block>),
    Ret(Exp),
    Cond(Exp, Box<Stmt>, Option<Box<Stmt>>),
}

impl DumpIR for Stmt {
    fn dump_ir(&self, sym_table: &mut SymTable, block_graph: &mut BlockGraph) -> String {
        match self {
            Stmt::Block(block) => {
                let ir: String = block.dump_ir(sym_table, block_graph);
                ir
            }
            Stmt::Exp(exp) => {
                if let Some(exp) = exp {
                    let ir = exp.dump_ir(sym_table, block_graph);
                    block_graph.insert_ir(&ir);
                    return ir;
                }
                String::new()
            }
            Stmt::SetLVal(lval, exp) => {
                let mut ir: String = String::new();
                let ident = match sym_table.search_sym(lval.ident.clone()) {
                    Sym::VarInt((ident, _)) => ident.clone(),
                    Sym::ConstInt(_) => panic!("attempt to change const value!"),
                    _ => panic!("symbol don't match"),
                };

                let pre_cnt = sym_table.count;
                let exp_ir = exp.dump_ir(sym_table, block_graph);
                let stmt_cnt = sym_table.count;

                if pre_cnt == stmt_cnt {
                    ir += &format!("\tstore {}, {}\n", exp_ir, ident);
                } else {
                    ir += &format!(
                        "{}\tstore {}, {}\n",
                        exp_ir,
                        sym_table.find_tsym(stmt_cnt - 1).unwrap(),
                        ident
                    );
                }
                block_graph.insert_ir(&ir);
                ir
            }
            Stmt::Ret(exp) => {
                let mut ir: String = String::new();
                let pre_cnt = sym_table.count;
                let exp_ir: String = exp.dump_ir(sym_table, block_graph);
                let ret_cnt = sym_table.count;
                if pre_cnt == ret_cnt {
                    ir += &format!("\tret {}\n", exp_ir)
                } else {
                    ir += &format!("{}\tret %{}\n", exp_ir, ret_cnt - 1)
                };
                block_graph.insert_ir(&ir);

                block_graph.end(None);
                ir
            }
            Stmt::Cond(cond, cons, alter) => {
                let mut ir: String = String::new();

                let pre_cnt = sym_table.count;
                let exp_ir = cond.dump_ir(sym_table, block_graph);
                let cond_cnt = sym_table.count;

                let cons_nm: String = sym_table.get_block_name();
                let end_nm: String = sym_table.get_block_name();
                let alter_nm: String = if alter.is_none() {
                    end_nm.clone()
                } else {
                    sym_table.get_block_name()
                };

                if pre_cnt == cond_cnt {
                    ir += &format!("\tbr {cond}, %{cons_nm}, %{alter_nm}\n", cond = exp_ir);
                } else {
                    ir += &format!(
                        "{prev_ir}\tbr %{cond}, %{cons_nm}, %{alter_nm}\n",
                        prev_ir = exp_ir,
                        cond = cond_cnt - 1,
                    );
                }
                block_graph.insert_ir(&ir);

                block_graph.insert_next(cons_nm.clone());
                block_graph.insert_next(alter_nm.clone());
                block_graph.end(Some(cons_nm.clone()));

                let mut cons_ir = cons.dump_ir(sym_table, block_graph);

                if !block_graph.is_end() {
                    cons_ir += &format!("\tjump %{end_nm}\n");

                    block_graph.insert_ir(&format!("\tjump %{end_nm}\n"));
                    block_graph.insert_next(end_nm.clone());
                    block_graph.end(None);
                }

                match alter {
                    Some(alter) => {
                        block_graph.insert(alter_nm.clone());

                        let mut alter_ir = alter.dump_ir(sym_table, block_graph);

                        if !block_graph.is_end() {
                            alter_ir += &format!("\tjump %{end_nm}\n");

                            block_graph.insert_ir(&format!("\tjump %{end_nm}\n"));
                            block_graph.insert_next(end_nm.clone());
                            block_graph.end(None);
                        }

                        ir += &format!("%{cons_nm}:\n{cons_ir}%{alter_nm}:\n{alter_ir}");
                    }
                    None => {
                        ir += &format!("%{cons_nm}:\n{cons_ir}");
                    }
                };

                block_graph.insert(end_nm.clone());
                ir += &format!("%{end_nm}:\n");

                String::new()
            } //_ => String::new(),
        }
    }
}
