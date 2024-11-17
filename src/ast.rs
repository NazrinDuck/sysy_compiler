use std::sync::atomic::{AtomicUsize, Ordering};

pub trait DumpIR {
    fn dump_ir(&self) -> String;
}

static COUNT: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

impl DumpIR for CompUnit {
    fn dump_ir(&self) -> String {
        self.func_def.dump_ir()
    }
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

impl DumpIR for FuncDef {
    fn dump_ir(&self) -> String {
        format!(
            "fun @{ident}(): {func_type} {{{block}}}",
            ident = self.ident,
            func_type = self.func_type.dump_ir(),
            block = self.block.dump_ir()
        )
    }
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

impl DumpIR for FuncType {
    fn dump_ir(&self) -> String {
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
    fn dump_ir(&self) -> String {
        match self {
            PrimaryExp::Exp(exp) => exp.dump_ir(),
            PrimaryExp::Number(num) => num.to_string(),
        }
    }
}

#[derive(Debug)]
pub struct Exp {
    pub unary_exp: UnaryExp,
}

impl DumpIR for Exp {
    fn dump_ir(&self) -> String {
        self.unary_exp.dump_ir()
    }
}

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(Box<PrimaryExp>),
    Unary((UnaryOp, Box<UnaryExp>)),
}

impl DumpIR for UnaryExp {
    fn dump_ir(&self) -> String {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => (*primary_exp).dump_ir(),
            UnaryExp::Unary((unary_op, unary_exp)) => {
                let unary_exp_ir: String = (*unary_exp).dump_ir();

                let mut unary_ir: String;

                let cnt = COUNT.load(Ordering::Relaxed);
                if !unary_exp_ir.contains("\n") {
                    if cnt == 0 {
                        match *unary_op {
                            UnaryOp::Positive => {
                                unary_ir = unary_exp_ir.to_string();
                            }
                            UnaryOp::Negative => {
                                unary_ir =
                                    format!("\t%{} = sub 0, {}\n", cnt, unary_exp_ir,).to_string();
                                COUNT.fetch_add(1, Ordering::Relaxed);
                            }
                            UnaryOp::Not => {
                                unary_ir = format!("\t%{} = eq {}, 0\n", cnt, unary_exp_ir);
                                COUNT.fetch_add(1, Ordering::Relaxed);
                            }
                        };
                    } else {
                        match *unary_op {
                            UnaryOp::Positive => {
                                unary_ir = unary_exp_ir.to_string();
                            }
                            UnaryOp::Negative => {
                                unary_ir = format!("\t%{} = sub {}, %{}\n", cnt, unary_exp_ir, cnt)
                                    .to_string();
                                COUNT.fetch_add(1, Ordering::Relaxed);
                            }
                            UnaryOp::Not => {
                                unary_ir = format!("\t%{} = eq {}, 0\n", cnt, unary_exp_ir);
                                COUNT.fetch_add(1, Ordering::Relaxed);
                            }
                        };
                    }
                } else {
                    unary_ir = unary_exp_ir.to_string();

                    match *unary_op {
                        UnaryOp::Positive => (),
                        UnaryOp::Negative => {
                            unary_ir += &format!("\t%{} = sub 0, %{}\n", cnt, cnt - 1).to_string();
                            COUNT.fetch_add(1, Ordering::Relaxed);
                        }
                        UnaryOp::Not => {
                            unary_ir += &format!("\t%{} = eq %{}, 0\n", cnt, cnt - 1);
                            COUNT.fetch_add(1, Ordering::Relaxed);
                        }
                    };
                };

                unary_ir
            }
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Positive,
    Negative,
    Not,
}

#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

impl DumpIR for Block {
    fn dump_ir(&self) -> String {
        let stmt_ir: String = self.stmt.dump_ir();
        let cnt = COUNT.load(Ordering::Relaxed);
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
    fn dump_ir(&self) -> String {
        self.exp.dump_ir()
    }
}
