pub trait DumpIR {
    fn dump_ir(&self) -> String;
}

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
pub struct Block {
    pub stmt: Stmt,
}

impl DumpIR for Block {
    fn dump_ir(&self) -> String {
        format!(
            r"
%enter:
    ret {}
",
            self.stmt.dump_ir()
        )
    }
}

pub type IntConst = i32;

/*
#[derive(Debug)]
pub struct Number {
    pub int_const: IntConst,
}
*/

#[derive(Debug)]
pub struct Stmt {
    pub num: i32,
}

impl DumpIR for Stmt {
    fn dump_ir(&self) -> String {
        self.num.to_string()
    }
}
