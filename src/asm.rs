use koopa::ir::{dfg::DataFlowGraph, BasicBlock, TypeKind, Value, ValueKind};
use std::collections::HashMap;

pub trait DumpAsm {
    fn dump_asm<'a>(&'a self, reg_allocator: &mut RegAllocator<'a>) -> String;
}

pub trait DumpReg {
    fn dump_reg(&self, asm: &mut String, reg_allocator: &mut RegAllocator) -> String;
}

pub trait ParseStack {
    fn parse_stack<'a>(&'a self, reg_allocator: &mut RegAllocator<'a>);
}

struct Reg {
    reg: String,
    is_use: bool,
    value: Option<Value>,
}

#[derive(Debug)]
struct StackFrame {
    space: u32,
    vars: HashMap<Value, u32>,
}

impl StackFrame {
    fn new() -> Self {
        StackFrame {
            space: 0,
            vars: HashMap::new(),
        }
    }
}

pub struct RegAllocator<'a> {
    regs: Vec<Reg>,
    dfg: Option<&'a DataFlowGraph>,
    stack_frame: StackFrame,
}

impl RegAllocator<'_> {
    pub fn new() -> Self {
        RegAllocator {
            regs: vec![
                Reg {
                    reg: "t0".to_string(),
                    is_use: false,
                    value: None,
                },
                Reg {
                    reg: "t1".to_string(),
                    is_use: false,
                    value: None,
                },
                Reg {
                    reg: "t2".to_string(),
                    is_use: false,
                    value: None,
                },
                Reg {
                    reg: "t3".to_string(),
                    is_use: false,
                    value: None,
                },
                Reg {
                    reg: "t4".to_string(),
                    is_use: false,
                    value: None,
                },
                Reg {
                    reg: "t5".to_string(),
                    is_use: false,
                    value: None,
                },
                Reg {
                    reg: "t6".to_string(),
                    is_use: false,
                    value: None,
                },
                Reg {
                    reg: "a0".to_string(),
                    is_use: false,
                    value: None,
                },
                Reg {
                    reg: "a1".to_string(),
                    is_use: false,
                    value: None,
                },
                Reg {
                    reg: "a2".to_string(),
                    is_use: false,
                    value: None,
                },
                Reg {
                    reg: "a3".to_string(),
                    is_use: false,
                    value: None,
                },
                Reg {
                    reg: "a4".to_string(),
                    is_use: false,
                    value: None,
                },
            ],
            dfg: None,
            stack_frame: StackFrame::new(),
        }
    }
    fn get_reg(&mut self, value: Value) -> Result<String, String> {
        match self.find_reg(value) {
            Ok(reg) => return Ok(reg.clone()),
            Err(_) => {
                for reg in &mut self.regs {
                    if !reg.is_use {
                        reg.value = Some(value);
                        reg.is_use = true;
                        return Ok(reg.reg.clone());
                    }
                }
            }
        };
        Err("can't find register not in use".to_string())
    }

    fn find_reg(&mut self, value: Value) -> Result<String, String> {
        for reg in &mut self.regs {
            if let Some(reg_value) = reg.value {
                if reg_value == value {
                    reg.is_use = true;
                    return Ok(reg.reg.clone());
                }
            }
        }
        Err("can't find fitable register".to_string())
    }

    fn free_reg(&mut self, value: Value) {
        for reg in &mut self.regs {
            if let Some(reg_value) = reg.value {
                if reg_value == value {
                    reg.is_use = false;
                }
            }
        }
    }

    fn get_block_name(&self, target: BasicBlock) -> String {
        match self.dfg.unwrap().bb(target).name() {
            Some(entry) if entry == "%entry" => String::from("main"),
            Some(block_name) => block_name[1..].to_string(),
            None => panic!("block has no name!"),
        }
    }
}

impl DumpAsm for koopa::ir::Program {
    fn dump_asm<'a>(&'a self, reg_allocator: &mut RegAllocator<'a>) -> String {
        let mut asm: String = String::new();
        for &func in self.func_layout() {
            let func_data = self.func(func);
            let func_name = &func_data.name()[1..];
            if func_name == "main" {
                asm.push_str(&format!("\t.globl {}\n", func_name));
            }
            asm.push_str(&format!("{}:\n", func_name));
            asm.push_str(&func_data.dump_asm(reg_allocator));
        }
        format!("\t.text\n{}", asm)
    }
}

impl ParseStack for koopa::ir::FunctionData {
    fn parse_stack<'a>(&'a self, reg_allocator: &mut RegAllocator<'a>) {
        let mut space: u32 = 0;
        for (&_, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);
                if let ValueKind::Alloc(_) = value_data.kind() {
                    match value_data.ty().kind() {
                        TypeKind::Pointer(_) => {
                            space += 4;
                            reg_allocator.stack_frame.vars.insert(inst, space);
                        }
                        _ => {
                            //println!("{}", other);
                        }
                    }
                }
            }
        }
        reg_allocator.stack_frame.space =
            (space & (!0xf)) + if (space & (0xf)) != 0 { 0x10 } else { 0 };
        dbg!(&reg_allocator.stack_frame);
    }
}

impl DumpAsm for koopa::ir::FunctionData {
    fn dump_asm<'a>(&'a self, reg_allocator: &mut RegAllocator<'a>) -> String {
        let mut asm: String = String::new();
        self.parse_stack(reg_allocator);
        let space: u32 = reg_allocator.stack_frame.space;
        if space <= 2048 && space > 0 {
            asm += &format!("\taddi\tsp, sp, -{}\n", space);
        } else if space != 0 {
            asm += &format!("\tli\tt0, -{}\n\taddi\tsp, sp, t0\n", space);
        }
        for (&bb, node) in self.layout().bbs() {
            reg_allocator.dfg = Some(self.dfg());
            let basic_block_data = self.dfg().bb(bb);
            //println!("{:#?}", basic_block_data.name());
            let name: String = match basic_block_data.name() {
                Some(entry) if entry == "%entry" => String::new(),
                Some(block_name) => format!("{}:\n", &block_name[1..]),
                None => panic!("block has no name!"),
            };
            asm += &name;

            for inst in node.insts().keys() {
                asm += &inst.dump_asm(reg_allocator);
            }
        }
        asm
    }
}

impl DumpAsm for Value {
    fn dump_asm<'a>(&'a self, reg_allocator: &mut RegAllocator<'a>) -> String {
        let mut asm: String = String::new();
        let dfg = reg_allocator.dfg.unwrap();
        let inst = *self;
        let value_data = dfg.value(inst);
        dbg!(value_data);
        match value_data.kind() {
            ValueKind::Integer(_) => (),
            ValueKind::Jump(jump) => {
                let mut ret_asm: String = String::new();
                let jump_name: String = reg_allocator.get_block_name(jump.target());
                ret_asm += &format!("\tj\t{}\n", jump_name);
                asm.push_str(&ret_asm);
            }
            ValueKind::Branch(branch) => {
                let mut ret_asm: String = String::new();
                let true_bb_name: String = reg_allocator.get_block_name(branch.true_bb());
                let false_bb_name: String = reg_allocator.get_block_name(branch.false_bb());

                let cond_reg: String = branch.cond().dump_reg(&mut ret_asm, reg_allocator);
                ret_asm += &format!("\tbnez\t{}, {}\n", cond_reg, true_bb_name);
                ret_asm += &format!("\tj\t{}\n", false_bb_name);
                asm.push_str(&ret_asm);
            }
            ValueKind::Store(store) => {
                let mut ret_asm: String = String::new();
                let value_reg: String = store.value().dump_reg(&mut ret_asm, reg_allocator);
                ret_asm += &format!(
                    "\tsw\t{rd}, {pos}(sp)\n",
                    rd = value_reg,
                    pos = reg_allocator.stack_frame.vars[&store.dest()]
                );
                reg_allocator.free_reg(store.value());
                asm.push_str(&ret_asm);
            }

            ValueKind::Return(ret) => {
                let mut ret_asm: String = String::new();
                let value_data = dfg.value(ret.value().unwrap());
                match value_data.kind() {
                    ValueKind::Integer(int) => {
                        ret_asm = format!("\tli\ta0, {}\n", int.value());
                    }
                    ValueKind::Binary(_) => {
                        ret_asm = format!(
                            "\tmv\ta0, {}\n",
                            reg_allocator.find_reg(ret.value().unwrap()).unwrap()
                        );
                    }
                    ValueKind::Load(load) => {
                        ret_asm = format!(
                            "\tlw\ta0, {pos}(sp)\n",
                            pos = reg_allocator.stack_frame.vars[&load.src()]
                        );
                    }
                    _ => (),
                }
                asm.push_str(&ret_asm);
                let space: u32 = reg_allocator.stack_frame.space;
                if space <= 2048 && space > 0 {
                    asm += &format!("\taddi\tsp, sp, {}\n", space);
                } else if space != 0 {
                    asm += &format!("\tli\tt0, {}\n\taddi\tsp, sp, t0\n", space);
                }
                asm += "\tret\n";
            }
            ValueKind::Binary(binary) => {
                let mut ret_asm: String = String::new();
                let self_reg: String = reg_allocator.get_reg(inst).unwrap();
                let lhs_reg: String = binary.lhs().dump_reg(&mut ret_asm, reg_allocator);
                let rhs_reg: String = binary.rhs().dump_reg(&mut ret_asm, reg_allocator);
                match binary.op() {
                    koopa::ir::BinaryOp::Eq => {
                        ret_asm += &format!(
                            "\txor\t{rd}, {rs1}, {rs2}\n\tseqz\t{rd}, {rd}\n",
                            rd = self_reg,
                            rs1 = lhs_reg,
                            rs2 = rhs_reg,
                        );
                    }
                    koopa::ir::BinaryOp::Sub => {
                        ret_asm += &format!(
                            "\tsub\t{rd}, {rs1}, {rs2}\n",
                            rd = self_reg,
                            rs1 = lhs_reg,
                            rs2 = rhs_reg
                        );
                    }
                    koopa::ir::BinaryOp::Add => {
                        ret_asm += &format!(
                            "\tadd\t{rd}, {rs1}, {rs2}\n",
                            rd = self_reg,
                            rs1 = lhs_reg,
                            rs2 = rhs_reg
                        );
                    }
                    koopa::ir::BinaryOp::Mul => {
                        ret_asm += &format!(
                            "\tmul\t{rd}, {rs1}, {rs2}\n",
                            rd = self_reg,
                            rs1 = lhs_reg,
                            rs2 = rhs_reg
                        );
                    }
                    koopa::ir::BinaryOp::Div => {
                        ret_asm += &format!(
                            "\tdiv\t{rd}, {rs1}, {rs2}\n",
                            rd = self_reg,
                            rs1 = lhs_reg,
                            rs2 = rhs_reg
                        );
                    }
                    koopa::ir::BinaryOp::Mod => {
                        ret_asm += &format!(
                            "\trem\t{rd}, {rs1}, {rs2}\n",
                            rd = self_reg,
                            rs1 = lhs_reg,
                            rs2 = rhs_reg
                        );
                    }
                    koopa::ir::BinaryOp::NotEq => {
                        ret_asm += &format!(
                            "\txor\t{rd}, {rs1}, {rs2}\n\tsnez\t{rd}, {rd}\n",
                            rd = self_reg,
                            rs1 = lhs_reg,
                            rs2 = rhs_reg
                        );
                    }
                    koopa::ir::BinaryOp::Gt => {
                        ret_asm += &format!(
                            "\tsgt\t{rd}, {rs1}, {rs2}\n",
                            rd = self_reg,
                            rs1 = lhs_reg,
                            rs2 = rhs_reg
                        );
                    }
                    koopa::ir::BinaryOp::Ge => {
                        ret_asm += &format!(
                            "\tslt\t{rd}, {rs1}, {rs2}\n\tseqz\t{rd}, {rd}\n",
                            rd = self_reg,
                            rs1 = lhs_reg,
                            rs2 = rhs_reg
                        );
                    }
                    koopa::ir::BinaryOp::Lt => {
                        ret_asm += &format!(
                            "\tslt\t{rd}, {rs1}, {rs2}\n",
                            rd = self_reg,
                            rs1 = lhs_reg,
                            rs2 = rhs_reg
                        );
                    }
                    koopa::ir::BinaryOp::Le => {
                        ret_asm += &format!(
                            "\tsgt\t{rd}, {rs1}, {rs2}\n\tseqz\t{rd}, {rd}\n",
                            rd = self_reg,
                            rs1 = lhs_reg,
                            rs2 = rhs_reg
                        );
                    }
                    koopa::ir::BinaryOp::Or => {
                        ret_asm += &format!(
                            "\tor\t{rd}, {rs1}, {rs2}\n",
                            rd = self_reg,
                            rs1 = lhs_reg,
                            rs2 = rhs_reg
                        );
                    }
                    _ => unreachable!(),
                }
                reg_allocator.free_reg(binary.lhs());
                reg_allocator.free_reg(binary.rhs());
                //dbg!(lhs_data);
                //dbg!(rhs_data);
                asm.push_str(&ret_asm);
            }
            _ => (),
        };
        asm
    }
}

impl DumpReg for Value {
    fn dump_reg(&self, asm: &mut String, reg_allocator: &mut RegAllocator) -> String {
        let dfg = reg_allocator.dfg.unwrap();
        let data = dfg.value(*self);
        let reg: String;

        match data.kind() {
            ValueKind::Integer(int) => {
                if int.value() == 0 {
                    reg = "x0".to_string();
                } else {
                    reg = reg_allocator.get_reg(*self).unwrap();
                    asm.push_str(&format!("\tli\t{rd}, {imm}\n", rd = reg, imm = int.value()));
                }
            }
            ValueKind::Binary(_) => {
                reg = reg_allocator.find_reg(*self).unwrap();
            }
            ValueKind::Load(load) => {
                reg = reg_allocator.get_reg(*self).unwrap();
                asm.push_str(&format!(
                    "\tlw\t{rd}, {pos}(sp)\n",
                    rd = reg,
                    pos = reg_allocator.stack_frame.vars[&load.src()]
                ));
            }
            _ => unreachable!(),
        }
        reg
    }
}

/*
  .text         # 声明之后的数据需要被放入代码段中
  .globl main   # 声明全局符号 main, 以便链接器处理
main:           # 标记 main 的入口点
  li a0, 0      # 将整数 0 加载到存放返回值的 a0 寄存器中
  ret           # 返回
*/
