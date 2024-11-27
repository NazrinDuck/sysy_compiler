use std::collections::HashMap;

pub trait DumpIR {
    fn dump_ir(&self, sym_table: &mut SymTable) -> String;
}

pub trait ParseSym {
    fn parse_sym(&self, sym_table: &mut SymTable);
}

pub trait DumpVal {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32;
}

struct SymMaps {
    int_map: HashMap<String, i32>,
    str_map: HashMap<String, String>,
}

impl SymMaps {
    fn new() -> Self {
        SymMaps {
            int_map: HashMap::new(),
            str_map: HashMap::new(),
        }
    }
}

pub struct SymTable {
    symbols: SymMaps,
    tem_symbols: Vec<(String, bool, Option<u32>)>,
    count: u32,
}

impl SymTable {
    pub fn new() -> Self {
        SymTable {
            symbols: SymMaps::new(),
            tem_symbols: Vec::new(),
            count: 0,
        }
    }

    fn get_new_sym(&mut self) -> String {
        let symbol: String = format!("%{}", self.count);
        self.tem_symbols
            .push((symbol.clone(), true, Some(self.count)));
        self.count += 1;
        symbol
    }

    fn find_sym(&self, val: u32) -> Result<String, String> {
        for sym in self.tem_symbols.clone() {
            if let Some(sym_val) = sym.2 {
                if sym_val == val && sym.1 {
                    return Ok(sym.0);
                }
            }
        }
        Err(format!("can't find fitable temporary symbols: {}", val).to_string())
    }
}

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

impl DumpIR for CompUnit {
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        self.func_def.dump_ir(sym_table)
    }
}

#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
}

impl DumpIR for Decl {
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        match self {
            Decl::ConstDecl(const_decl) => {
                const_decl.parse_sym(sym_table);
                "".to_string()
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
                    sym_table
                        .symbols
                        .int_map
                        .insert(const_def.ident.clone(), val);
                }
                _ => (),
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
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        format!(
            "fun @{ident}(): {func_type} {{{block}}}",
            ident = self.ident,
            func_type = self.func_type.dump_ir(sym_table),
            block = self.block.dump_ir(sym_table)
        )
    }
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

impl DumpIR for FuncType {
    fn dump_ir(&self, _: &mut SymTable) -> String {
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
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        match self {
            PrimaryExp::Exp(exp) => exp.dump_ir(sym_table),
            PrimaryExp::LVal(lval) => sym_table.symbols.int_map[&lval.ident].to_string(),
            PrimaryExp::Number(num) => num.to_string(),
        }
    }
}

impl DumpVal for PrimaryExp {
    fn dump_val(&self, sym_table: &mut SymTable) -> i32 {
        match self {
            PrimaryExp::Exp(exp) => exp.dump_val(sym_table),
            PrimaryExp::LVal(lval) => sym_table.symbols.int_map[&lval.ident],
            PrimaryExp::Number(num) => *num,
        }
    }
}

#[derive(Debug)]
pub struct Exp {
    pub lor_exp: LOrExp,
}

impl DumpIR for Exp {
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        self.lor_exp.dump_ir(sym_table)
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
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        match self {
            Self::PrimaryExp(primary_exp) => primary_exp.dump_ir(sym_table),
            Self::Exp((unary_op, unary_exp)) => {
                let pre_sym = sym_table.count;
                let unary_exp_ir: String = unary_exp.dump_ir(sym_table);
                let unary_sym = sym_table.count;

                let mut ir: String;

                if unary_sym == pre_sym {
                    match unary_op {
                        UnaryOp::Positive => {
                            ir = unary_exp_ir.to_string();
                        }
                        UnaryOp::Negative => {
                            ir = format!(
                                "\t{} = sub 0, {}\n",
                                sym_table.get_new_sym(),
                                unary_exp_ir
                            );
                        }
                        UnaryOp::Not => {
                            ir =
                                format!("\t{} = eq {}, 0\n", sym_table.get_new_sym(), unary_exp_ir);
                        }
                    }
                } else {
                    ir = unary_exp_ir.to_string();
                    match unary_op {
                        UnaryOp::Positive => (),
                        UnaryOp::Negative => {
                            ir += &format!(
                                "\t{} = sub 0, {}\n",
                                sym_table.get_new_sym(),
                                sym_table.find_sym(unary_sym - 1).unwrap()
                            );
                        }
                        UnaryOp::Not => {
                            ir += &format!(
                                "\t{} = eq {}, 0\n",
                                sym_table.get_new_sym(),
                                sym_table.find_sym(unary_sym - 1).unwrap()
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
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        match self {
            Self::UnaryExp(unary_exp) => unary_exp.dump_ir(sym_table),
            Self::Exp((mul_exp, mul_op, unary_exp)) => {
                let pre_cnt = sym_table.count;
                let mul_exp_ir: String = mul_exp.dump_ir(sym_table);
                let lhs_cnt = sym_table.count;
                let unary_exp_ir: String = unary_exp.dump_ir(sym_table);
                let rhs_cnt = sym_table.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = mul_exp_ir;
                } else {
                    lhs_sym = sym_table.find_sym(lhs_cnt - 1).unwrap();
                    ir += &mul_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = unary_exp_ir;
                } else {
                    rhs_sym = sym_table.find_sym(rhs_cnt - 1).unwrap();
                    ir += &unary_exp_ir.to_string();
                }

                match mul_op {
                    MulOp::Mul => {
                        ir += &format!(
                            "\t{} = mul {}, {}\n",
                            sym_table.get_new_sym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    MulOp::Div => {
                        ir += &format!(
                            "\t{} = div {}, {}\n",
                            sym_table.get_new_sym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    MulOp::Mod => {
                        ir += &format!(
                            "\t{} = mod {}, {}\n",
                            sym_table.get_new_sym(),
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
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        match self {
            Self::MulExp(mul_exp) => (*mul_exp).dump_ir(sym_table),
            Self::Exp((add_exp, add_op, mul_exp)) => {
                let pre_cnt = sym_table.count;
                let add_exp_ir: String = add_exp.dump_ir(sym_table);
                let lhs_cnt = sym_table.count;
                let mul_exp_ir: String = mul_exp.dump_ir(sym_table);
                let rhs_cnt = sym_table.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = add_exp_ir;
                } else {
                    lhs_sym = sym_table.find_sym(lhs_cnt - 1).unwrap();
                    ir += &add_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = mul_exp_ir;
                } else {
                    rhs_sym = sym_table.find_sym(rhs_cnt - 1).unwrap();
                    ir += &mul_exp_ir.to_string();
                }

                match add_op {
                    AddOp::Add => {
                        ir += &format!(
                            "\t{} = add {}, {}\n",
                            sym_table.get_new_sym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    AddOp::Sub => {
                        ir += &format!(
                            "\t{} = sub {}, {}\n",
                            sym_table.get_new_sym(),
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
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        match self {
            Self::AddExp(add_exp) => add_exp.dump_ir(sym_table),
            Self::Exp((rel_exp, rel_op, add_exp)) => {
                let pre_cnt = sym_table.count;
                let rel_exp_ir: String = rel_exp.dump_ir(sym_table);
                let lhs_cnt = sym_table.count;
                let add_exp_ir: String = add_exp.dump_ir(sym_table);
                let rhs_cnt = sym_table.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = rel_exp_ir;
                } else {
                    lhs_sym = sym_table.find_sym(lhs_cnt - 1).unwrap();
                    ir += &rel_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = add_exp_ir;
                } else {
                    rhs_sym = sym_table.find_sym(rhs_cnt - 1).unwrap();
                    ir += &add_exp_ir.to_string();
                }

                match rel_op {
                    RelOp::Gt => {
                        ir += &format!(
                            "\t{} = gt {}, {}\n",
                            sym_table.get_new_sym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    RelOp::Lt => {
                        ir += &format!(
                            "\t{} = lt {}, {}\n",
                            sym_table.get_new_sym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    RelOp::Ge => {
                        ir += &format!(
                            "\t{} = ge {}, {}\n",
                            sym_table.get_new_sym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    RelOp::Le => {
                        ir += &format!(
                            "\t{} = le {}, {}\n",
                            sym_table.get_new_sym(),
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
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        match self {
            Self::RelExp(rel_exp) => rel_exp.dump_ir(sym_table),
            Self::Exp((eq_exp, eq_op, rel_exp)) => {
                let pre_cnt = sym_table.count;
                let eq_exp_ir: String = eq_exp.dump_ir(sym_table);
                let lhs_cnt = sym_table.count;
                let rel_exp_ir: String = rel_exp.dump_ir(sym_table);
                let rhs_cnt = sym_table.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = eq_exp_ir;
                } else {
                    lhs_sym = sym_table.find_sym(lhs_cnt - 1).unwrap();
                    ir += &eq_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = rel_exp_ir;
                } else {
                    rhs_sym = sym_table.find_sym(rhs_cnt - 1).unwrap();
                    ir += &rel_exp_ir.to_string();
                }

                match eq_op {
                    EqOp::Eq => {
                        ir += &format!(
                            "\t{} = eq {}, {}\n",
                            sym_table.get_new_sym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    EqOp::NotEq => {
                        ir += &format!(
                            "\t{} = ne {}, {}\n",
                            sym_table.get_new_sym(),
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
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        match self {
            Self::EqExp(eq_exp) => eq_exp.dump_ir(sym_table),
            Self::Exp((land_exp, eq_exp)) => {
                let pre_cnt = sym_table.count;
                let land_exp_ir: String = land_exp.dump_ir(sym_table);
                let lhs_cnt = sym_table.count;
                let eq_exp_ir: String = eq_exp.dump_ir(sym_table);
                let rhs_cnt = sym_table.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = land_exp_ir;
                } else {
                    lhs_sym = sym_table.find_sym(lhs_cnt - 1).unwrap();
                    ir += &land_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = eq_exp_ir;
                } else {
                    rhs_sym = sym_table.find_sym(rhs_cnt - 1).unwrap();
                    ir += &eq_exp_ir.to_string();
                }

                let tem_res = sym_table.get_new_sym();
                ir += &format!(
                    "\t{tem_res} = mul {}, {}\n\t{} = ne {tem_res}, 0\n",
                    lhs_sym,
                    rhs_sym,
                    sym_table.get_new_sym(),
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
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        match self {
            Self::LAndExp(land_exp) => land_exp.dump_ir(sym_table),
            Self::Exp((lor_exp, land_exp)) => {
                let pre_cnt = sym_table.count;
                let lor_exp_ir: String = lor_exp.dump_ir(sym_table);
                let lhs_cnt = sym_table.count;
                let land_exp_ir: String = land_exp.dump_ir(sym_table);
                let rhs_cnt = sym_table.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = lor_exp_ir;
                } else {
                    lhs_sym = sym_table.find_sym(lhs_cnt - 1).unwrap();
                    ir += &lor_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = land_exp_ir;
                } else {
                    rhs_sym = sym_table.find_sym(rhs_cnt - 1).unwrap();
                    ir += &land_exp_ir.to_string();
                }

                let tem_res = sym_table.get_new_sym();
                ir += &format!(
                    "\t{tem_res} = or {}, {}\n\t{} = ne {tem_res}, 0\n",
                    lhs_sym,
                    rhs_sym,
                    sym_table.get_new_sym(),
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
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        let mut ir: String = String::new();
        for block_item in &self.block_items {
            ir += &block_item.dump_ir(sym_table);
        }
        ir
    }
}

#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

impl DumpIR for BlockItem {
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        match self {
            BlockItem::Decl(decl) => decl.dump_ir(sym_table),
            BlockItem::Stmt(stmt) => {
                let stmt_ir: String = stmt.dump_ir(sym_table);
                let cnt = sym_table.count;
                if cnt == 0 {
                    format!("\n%enter:\n\tret {}\n", stmt_ir)
                } else {
                    format!("\n%enter:\n{}\tret %{}\n", stmt_ir, cnt - 1)
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub exp: Exp,
}

impl DumpIR for Stmt {
    fn dump_ir(&self, sym_table: &mut SymTable) -> String {
        self.exp.dump_ir(sym_table)
    }
}
