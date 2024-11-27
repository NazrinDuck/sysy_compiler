pub trait DumpIR {
    fn dump_ir(&self, tem_syms: &mut TemSyms) -> String;
}

pub struct TemSyms {
    symbols: Vec<(String, bool, Option<u32>)>,
    count: u32,
}

impl TemSyms {
    pub fn new() -> Self {
        TemSyms {
            symbols: Vec::new(),
            count: 0,
        }
    }

    fn get_new_sym(&mut self) -> String {
        let symbol: String = format!("%{}", self.count);
        self.symbols.push((symbol.clone(), true, Some(self.count)));
        self.count += 1;
        symbol
    }

    fn find_sym(&self, val: u32) -> Result<String, String> {
        for sym in self.symbols.clone() {
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
    fn dump_ir(&self, tem_syms: &mut TemSyms) -> String {
        self.func_def.dump_ir(tem_syms)
    }
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

impl DumpIR for FuncDef {
    fn dump_ir(&self, tem_syms: &mut TemSyms) -> String {
        format!(
            "fun @{ident}(): {func_type} {{{block}}}",
            ident = self.ident,
            func_type = self.func_type.dump_ir(tem_syms),
            block = self.block.dump_ir(tem_syms)
        )
    }
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

impl DumpIR for FuncType {
    fn dump_ir(&self, _: &mut TemSyms) -> String {
        match self {
            FuncType::Int => "i32".to_string(),
        }
    }
}

#[derive(Debug)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    Number(i32),
}

impl DumpIR for PrimaryExp {
    fn dump_ir(&self, tem_syms: &mut TemSyms) -> String {
        match self {
            PrimaryExp::Exp(exp) => exp.dump_ir(tem_syms),
            PrimaryExp::Number(num) => num.to_string(),
        }
    }
}

#[derive(Debug)]
pub struct Exp {
    pub lor_exp: LOrExp,
}

impl DumpIR for Exp {
    fn dump_ir(&self, tem_syms: &mut TemSyms) -> String {
        self.lor_exp.dump_ir(tem_syms)
    }
}

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(Box<PrimaryExp>),
    Exp((UnaryOp, Box<UnaryExp>)),
}

impl DumpIR for UnaryExp {
    fn dump_ir(&self, tem_syms: &mut TemSyms) -> String {
        match self {
            Self::PrimaryExp(primary_exp) => (*primary_exp).dump_ir(tem_syms),
            Self::Exp((unary_op, unary_exp)) => {
                let pre_sym = tem_syms.count;
                let unary_exp_ir: String = (*unary_exp).dump_ir(tem_syms);
                let unary_sym = tem_syms.count;

                let mut ir: String;

                if unary_sym == pre_sym {
                    match unary_op {
                        UnaryOp::Positive => {
                            ir = unary_exp_ir.to_string();
                        }
                        UnaryOp::Negative => {
                            ir =
                                format!("\t{} = sub 0, {}\n", tem_syms.get_new_sym(), unary_exp_ir);
                        }
                        UnaryOp::Not => {
                            ir = format!("\t{} = eq {}, 0\n", tem_syms.get_new_sym(), unary_exp_ir);
                        }
                    }
                } else {
                    ir = unary_exp_ir.to_string();
                    match unary_op {
                        UnaryOp::Positive => (),
                        UnaryOp::Negative => {
                            ir += &format!(
                                "\t{} = sub 0, {}\n",
                                tem_syms.get_new_sym(),
                                tem_syms.find_sym(unary_sym - 1).unwrap()
                            );
                        }
                        UnaryOp::Not => {
                            ir += &format!(
                                "\t{} = eq {}, 0\n",
                                tem_syms.get_new_sym(),
                                tem_syms.find_sym(unary_sym - 1).unwrap()
                            );
                        }
                    }
                }

                ir
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
    fn dump_ir(&self, tem_syms: &mut TemSyms) -> String {
        match self {
            Self::UnaryExp(unary_exp) => (*unary_exp).dump_ir(tem_syms),
            Self::Exp((mul_exp, mul_op, unary_exp)) => {
                let pre_cnt = tem_syms.count;
                let mul_exp_ir: String = (*mul_exp).dump_ir(tem_syms);
                let lhs_cnt = tem_syms.count;
                let unary_exp_ir: String = (*unary_exp).dump_ir(tem_syms);
                let rhs_cnt = tem_syms.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = mul_exp_ir;
                } else {
                    lhs_sym = tem_syms.find_sym(lhs_cnt - 1).unwrap();
                    ir += &mul_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = unary_exp_ir;
                } else {
                    rhs_sym = tem_syms.find_sym(rhs_cnt - 1).unwrap();
                    ir += &unary_exp_ir.to_string();
                }

                match mul_op {
                    MulOp::Mul => {
                        ir += &format!(
                            "\t{} = mul {}, {}\n",
                            tem_syms.get_new_sym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    MulOp::Div => {
                        ir += &format!(
                            "\t{} = div {}, {}\n",
                            tem_syms.get_new_sym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    MulOp::Mod => {
                        ir += &format!(
                            "\t{} = mod {}, {}\n",
                            tem_syms.get_new_sym(),
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

#[derive(Debug)]
pub enum AddExp {
    MulExp(Box<MulExp>),
    Exp((Box<AddExp>, AddOp, Box<MulExp>)),
}

impl DumpIR for AddExp {
    fn dump_ir(&self, tem_syms: &mut TemSyms) -> String {
        match self {
            Self::MulExp(mul_exp) => (*mul_exp).dump_ir(tem_syms),
            Self::Exp((add_exp, add_op, mul_exp)) => {
                let pre_cnt = tem_syms.count;
                let add_exp_ir: String = (*add_exp).dump_ir(tem_syms);
                let lhs_cnt = tem_syms.count;
                let mul_exp_ir: String = (*mul_exp).dump_ir(tem_syms);
                let rhs_cnt = tem_syms.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = add_exp_ir;
                } else {
                    lhs_sym = tem_syms.find_sym(lhs_cnt - 1).unwrap();
                    ir += &add_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = mul_exp_ir;
                } else {
                    rhs_sym = tem_syms.find_sym(rhs_cnt - 1).unwrap();
                    ir += &mul_exp_ir.to_string();
                }

                match add_op {
                    AddOp::Add => {
                        ir += &format!(
                            "\t{} = add {}, {}\n",
                            tem_syms.get_new_sym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    AddOp::Sub => {
                        ir += &format!(
                            "\t{} = sub {}, {}\n",
                            tem_syms.get_new_sym(),
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

#[derive(Debug)]
pub enum RelExp {
    AddExp(AddExp),
    Exp((Box<RelExp>, RelOp, AddExp)),
}

impl DumpIR for RelExp {
    fn dump_ir(&self, tem_syms: &mut TemSyms) -> String {
        match self {
            Self::AddExp(add_exp) => add_exp.dump_ir(tem_syms),
            Self::Exp((rel_exp, rel_op, add_exp)) => {
                let pre_cnt = tem_syms.count;
                let rel_exp_ir: String = (*rel_exp).dump_ir(tem_syms);
                let lhs_cnt = tem_syms.count;
                let add_exp_ir: String = add_exp.dump_ir(tem_syms);
                let rhs_cnt = tem_syms.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = rel_exp_ir;
                } else {
                    lhs_sym = tem_syms.find_sym(lhs_cnt - 1).unwrap();
                    ir += &rel_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = add_exp_ir;
                } else {
                    rhs_sym = tem_syms.find_sym(rhs_cnt - 1).unwrap();
                    ir += &add_exp_ir.to_string();
                }

                match rel_op {
                    RelOp::Gt => {
                        ir += &format!(
                            "\t{} = gt {}, {}\n",
                            tem_syms.get_new_sym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    RelOp::Lt => {
                        ir += &format!(
                            "\t{} = lt {}, {}\n",
                            tem_syms.get_new_sym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    RelOp::Ge => {
                        ir += &format!(
                            "\t{} = ge {}, {}\n",
                            tem_syms.get_new_sym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    RelOp::Le => {
                        ir += &format!(
                            "\t{} = le {}, {}\n",
                            tem_syms.get_new_sym(),
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

#[derive(Debug)]
pub enum EqExp {
    RelExp(RelExp),
    Exp((Box<EqExp>, EqOp, RelExp)),
}

impl DumpIR for EqExp {
    fn dump_ir(&self, tem_syms: &mut TemSyms) -> String {
        match self {
            Self::RelExp(rel_exp) => rel_exp.dump_ir(tem_syms),
            Self::Exp((eq_exp, eq_op, rel_exp)) => {
                let pre_cnt = tem_syms.count;
                let eq_exp_ir: String = (*eq_exp).dump_ir(tem_syms);
                let lhs_cnt = tem_syms.count;
                let rel_exp_ir: String = rel_exp.dump_ir(tem_syms);
                let rhs_cnt = tem_syms.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = eq_exp_ir;
                } else {
                    lhs_sym = tem_syms.find_sym(lhs_cnt - 1).unwrap();
                    ir += &eq_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = rel_exp_ir;
                } else {
                    rhs_sym = tem_syms.find_sym(rhs_cnt - 1).unwrap();
                    ir += &rel_exp_ir.to_string();
                }

                match eq_op {
                    EqOp::Eq => {
                        ir += &format!(
                            "\t{} = eq {}, {}\n",
                            tem_syms.get_new_sym(),
                            lhs_sym,
                            rhs_sym
                        );
                    }
                    EqOp::NotEq => {
                        ir += &format!(
                            "\t{} = ne {}, {}\n",
                            tem_syms.get_new_sym(),
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

#[derive(Debug)]
pub enum LAndExp {
    EqExp(EqExp),
    Exp((Box<LAndExp>, EqExp)),
}

impl DumpIR for LAndExp {
    fn dump_ir(&self, tem_syms: &mut TemSyms) -> String {
        match self {
            Self::EqExp(eq_exp) => eq_exp.dump_ir(tem_syms),
            Self::Exp((land_exp, eq_exp)) => {
                let pre_cnt = tem_syms.count;
                let land_exp_ir: String = (*land_exp).dump_ir(tem_syms);
                let lhs_cnt = tem_syms.count;
                let eq_exp_ir: String = eq_exp.dump_ir(tem_syms);
                let rhs_cnt = tem_syms.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = land_exp_ir;
                } else {
                    lhs_sym = tem_syms.find_sym(lhs_cnt - 1).unwrap();
                    ir += &land_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = eq_exp_ir;
                } else {
                    rhs_sym = tem_syms.find_sym(rhs_cnt - 1).unwrap();
                    ir += &eq_exp_ir.to_string();
                }

                let tem_res = tem_syms.get_new_sym();
                ir += &format!(
                    "\t{tem_res} = mul {}, {}\n\t{} = ne {tem_res}, 0\n",
                    lhs_sym,
                    rhs_sym,
                    tem_syms.get_new_sym(),
                );

                ir
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
    fn dump_ir(&self, tem_syms: &mut TemSyms) -> String {
        match self {
            Self::LAndExp(land_exp) => land_exp.dump_ir(tem_syms),
            Self::Exp((lor_exp, land_exp)) => {
                let pre_cnt = tem_syms.count;
                let lor_exp_ir: String = (*lor_exp).dump_ir(tem_syms);
                let lhs_cnt = tem_syms.count;
                let land_exp_ir: String = land_exp.dump_ir(tem_syms);
                let rhs_cnt = tem_syms.count;

                let mut ir: String = String::new();

                let lhs_sym: String;
                let rhs_sym: String;

                if pre_cnt == lhs_cnt {
                    lhs_sym = lor_exp_ir;
                } else {
                    lhs_sym = tem_syms.find_sym(lhs_cnt - 1).unwrap();
                    ir += &lor_exp_ir.to_string();
                }

                if lhs_cnt == rhs_cnt {
                    rhs_sym = land_exp_ir;
                } else {
                    rhs_sym = tem_syms.find_sym(rhs_cnt - 1).unwrap();
                    ir += &land_exp_ir.to_string();
                }

                let tem_res = tem_syms.get_new_sym();
                ir += &format!(
                    "\t{tem_res} = or {}, {}\n\t{} = ne {tem_res}, 0\n",
                    lhs_sym,
                    rhs_sym,
                    tem_syms.get_new_sym(),
                );

                ir
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
    pub stmt: Stmt,
}

impl DumpIR for Block {
    fn dump_ir(&self, tem_syms: &mut TemSyms) -> String {
        let stmt_ir: String = self.stmt.dump_ir(tem_syms);
        let cnt = tem_syms.count;
        if cnt == 0 {
            format!("\n%enter:\n\tret {}\n", stmt_ir)
        } else {
            format!("\n%enter:\n{}\tret %{}\n", stmt_ir, cnt - 1)
        }
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub exp: Exp,
}

impl DumpIR for Stmt {
    fn dump_ir(&self, tem_syms: &mut TemSyms) -> String {
        self.exp.dump_ir(tem_syms)
    }
}
