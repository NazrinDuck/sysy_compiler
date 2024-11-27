use koopa::ir::{dfg::DataFlowGraph, entities::ValueData, Value, ValueKind};

pub trait DumpAsm {
    fn dump_asm<'a>(&'a self, reg_allocator: &mut RegAllocator<'a>) -> String;
}

struct Reg {
    reg: String,
    is_use: bool,
    value: Option<Value>,
}

pub struct RegAllocator<'a> {
    regs: Vec<Reg>,
    dfg: Option<&'a DataFlowGraph>,
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

impl DumpAsm for koopa::ir::FunctionData {
    fn dump_asm<'a>(&'a self, reg_allocator: &mut RegAllocator<'a>) -> String {
        let mut asm: String = String::new();
        for (&bb, node) in self.layout().bbs() {
            reg_allocator.dfg = Some(self.dfg());
            let basic_block_data = self.dfg().bb(bb);
            println!("{:#?}", basic_block_data.name());

            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);
                dbg!(value_data);
                match value_data.kind() {
                    ValueKind::Integer(_) => (),

                    ValueKind::Return(ret) => {
                        let mut ret_asm: String = String::new();
                        let value_data = self.dfg().value(ret.value().unwrap());
                        match value_data.kind() {
                            ValueKind::Integer(int) => {
                                ret_asm = format!("\tli\ta0, {}\n\tret\n", int.value());
                            }
                            ValueKind::Binary(_) => {
                                ret_asm = format!(
                                    "\tmv\ta0, {}\n\tret\n",
                                    reg_allocator.find_reg(ret.value().unwrap()).unwrap()
                                );
                            }
                            _ => (),
                        }
                        asm.push_str(&ret_asm);
                    }
                    ValueKind::Binary(binary) => {
                        let mut ret_asm: String = String::new();
                        let lhs_reg: String;
                        let rhs_reg: String;
                        let self_reg: String = reg_allocator.get_reg(inst).unwrap();
                        let lhs_data = self.dfg().value(binary.lhs());
                        let rhs_data = self.dfg().value(binary.rhs());
                        match lhs_data.kind() {
                            ValueKind::Integer(int) => {
                                if int.value() == 0 {
                                    lhs_reg = "x0".to_string();
                                } else {
                                    lhs_reg = reg_allocator.get_reg(binary.lhs()).unwrap();
                                    ret_asm.push_str(&format!(
                                        "\tli\t{rd}, {imm}\n",
                                        rd = lhs_reg,
                                        imm = int.value()
                                    ));
                                }
                            }
                            ValueKind::Binary(_) => {
                                lhs_reg = reg_allocator.find_reg(binary.lhs()).unwrap();
                            }
                            _ => unreachable!(),
                        }
                        match rhs_data.kind() {
                            ValueKind::Integer(int) => {
                                if int.value() == 0 {
                                    rhs_reg = "x0".to_string();
                                } else {
                                    rhs_reg = reg_allocator.get_reg(binary.rhs()).unwrap();
                                    ret_asm.push_str(&format!(
                                        "\tli\t{rd}, {imm}\n",
                                        rd = rhs_reg,
                                        imm = int.value()
                                    ));
                                }
                            }
                            ValueKind::Binary(_) => {
                                rhs_reg = reg_allocator.find_reg(binary.rhs()).unwrap();
                            }
                            _ => unreachable!(),
                        }
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
                        dbg!(lhs_data);
                        dbg!(rhs_data);
                        asm.push_str(&ret_asm);
                    }
                    _ => {}
                }
            }
        }
        asm
    }
}

/*
impl DumpAsm for ValueData {
    fn dump_asm<'a>(&'a self, reg_allocator: &mut RegAllocator<'a>) -> String {
        let mut asm: String = String::new();
        let dfg = reg_allocator.dfg.unwrap();

        match self.kind() {
            ValueKind::Integer(_) => (),

            ValueKind::Return(ret) => {
                let mut ret_asm: String = String::new();
                let value_data = dfg.value(ret.value().unwrap());
                match value_data.kind() {
                    ValueKind::Integer(int) => {
                        ret_asm = format!("\tli\ta0, {}\n\tret\n", int.value());
                    }
                    ValueKind::Binary(_) => {
                        ret_asm = format!(
                            "\tmv\ta0, {}\n\tret\n",
                            reg_allocator.find_reg(ret.value().unwrap()).unwrap()
                        );
                    }
                    _ => (),
                }
                asm.push_str(&ret_asm);
            }
            ValueKind::Binary(binary) => {
                let mut ret_asm: String = String::new();
                let lhs_reg: String; // = String::new();
                let rhs_reg: String; // = String::new();
                let self_reg: String = reg_allocator.get_reg(inst).unwrap();
                let lhs_data = dfg.value(binary.lhs());
                let rhs_data = dfg.value(binary.rhs());
                match lhs_data.kind() {
                    ValueKind::Integer(int) => {
                        if int.value() == 0 {
                            lhs_reg = "x0".to_string();
                        } else {
                            lhs_reg = reg_allocator.get_reg(binary.lhs()).unwrap();
                            ret_asm.push_str(&format!(
                                "\tli\t{rd}, {imm}\n",
                                rd = lhs_reg,
                                imm = int.value()
                            ));
                        }
                    }
                    ValueKind::Binary(_) => {
                        lhs_reg = reg_allocator.find_reg(binary.lhs()).unwrap();
                    }
                    _ => unreachable!(),
                }
                match rhs_data.kind() {
                    ValueKind::Integer(int) => {
                        if int.value() == 0 {
                            rhs_reg = "x0".to_string();
                        } else {
                            rhs_reg = reg_allocator.get_reg(binary.rhs()).unwrap();
                            ret_asm.push_str(&format!(
                                "\tli\t{rd}, {imm}\n",
                                rd = rhs_reg,
                                imm = int.value()
                            ));
                        }
                    }
                    ValueKind::Binary(_) => {
                        rhs_reg = reg_allocator.find_reg(binary.rhs()).unwrap();
                    }
                    _ => unreachable!(),
                }
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
                    _ => unreachable!(),
                }
                reg_allocator.free_reg(binary.lhs());
                reg_allocator.free_reg(binary.rhs());
                dbg!(lhs_data);
                dbg!(rhs_data);
                asm.push_str(&ret_asm);
            }
            _ => {}
        };
        asm
    }
}
*/

/*
  .text         # 声明之后的数据需要被放入代码段中
  .globl main   # 声明全局符号 main, 以便链接器处理
main:           # 标记 main 的入口点
  li a0, 0      # 将整数 0 加载到存放返回值的 a0 寄存器中
  ret           # 返回
*/
