use koopa::ir::{Value, ValueKind};

pub trait DumpAsm {
    fn dump_asm(&self) -> String;
}

struct Reg {
    reg: String,
    is_use: bool,
    value: Option<Value>,
}

impl Reg {
    fn init_regs() -> Vec<Self> {
        vec![
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
        ]
    }

    fn get_reg(regs: &mut Vec<Reg>, value: Value) -> Result<String, String> {
        match Self::find_reg(regs, value) {
            Ok(reg) => return Ok(reg.clone()),
            Err(_) => {
                for reg in regs {
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

    fn find_reg(regs: &mut Vec<Reg>, value: Value) -> Result<String, String> {
        for reg in regs {
            if let Some(reg_value) = reg.value {
                if reg_value == value {
                    reg.is_use = true;
                    return Ok(reg.reg.clone());
                }
            }
        }
        Err("can't find fitable register".to_string())
    }

    fn free_reg(regs: &mut Vec<Reg>, value: Value) {
        for reg in regs {
            if let Some(reg_value) = reg.value {
                if reg_value == value {
                    reg.is_use = false;
                }
            }
        }
    }
}

/*
pub trait GetReg {
    fn get_reg(&self, asm: &mut String) -> String;
}

struct ValueReg {
    value_data: ValueData,
    reg: Reg,
}

enum Reg {
    Reg(String),
    Imm(i32),
    None,
}

static REG_COUNT: AtomicUsize = AtomicUsize::new(0);

impl ValueReg {
    fn new(&self, value_data: ValueData) -> Self {
        ValueReg {
            value_data,
            reg: Reg::None,
        }
    }

    fn alloc_reg(&mut self, dfg: &DataFlowGraph) {
        let cnt = REG_COUNT.load(Ordering::Relaxed);
        match self.value_data.kind() {
            ValueKind::Integer(int) => {
                if int.value() == 0 {
                    self.reg = Reg::Reg("x0".to_string());
                } else {
                    self.reg = Reg::Reg(format!("t{}", cnt));
                    REG_COUNT.fetch_add(1, Ordering::Relaxed);
                }
            }
            ValueKind::Binary(binary) => {
                let mut lhs_vr: ValueReg = self.new(dfg.value(binary.lhs()).to_owned());
                let mut rhs_vr: ValueReg = self.new(dfg.value(binary.rhs()).to_owned());
                lhs_vr.alloc_reg(dfg);
                rhs_vr.alloc_reg(dfg);
                self.reg = Reg::Reg(format!("t{}", cnt));
                REG_COUNT.fetch_add(1, Ordering::Relaxed);
            }
            _ => (),
        }
    }
}

pub trait Reg {
    const REG: &str;
    fn get_reg(&self, asm: &mut String) -> String;
    fn set_reg(&self, asm: &mut String) -> String;
}
impl Reg {
    fn get_reg(&mut self) -> String {
        self.count += 1;
        match self.count {
            0_u32..=6_u32 => format!("t{}", self.count),
            7_u32..=14_u32 => format!("a{}", self.count - 7),
            _ => unreachable!(),
        }
    }
}
*/

impl DumpAsm for koopa::ir::Program {
    fn dump_asm(&self) -> String {
        let mut asm: String = String::new();
        for &func in self.func_layout() {
            let func_data = self.func(func);
            let func_name = &func_data.name()[1..];
            if func_name == "main" {
                asm.push_str(&format!("\t.globl {}\n", func_name));
            }
            asm.push_str(&format!("{}:\n", func_name));
            asm.push_str(&func_data.dump_asm());
        }
        format!("\t.text\n{}", asm)
    }
}

impl DumpAsm for koopa::ir::FunctionData {
    fn dump_asm(&self) -> String {
        let mut regs: Vec<Reg> = Reg::init_regs();
        let mut asm: String = String::new();
        for (&bb, node) in self.layout().bbs() {
            let basic_block_data = self.dfg().bb(bb);
            println!("{:#?}", basic_block_data.name());
            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);
                dbg!(value_data);
                match value_data.kind() {
                    ValueKind::Integer(_) => (),

                    ValueKind::Return(ret) => {
                        let mut ret_asm: String = String::new();
                        let value_data = self.dfg().value(ret.value().expect(""));
                        match value_data.kind() {
                            ValueKind::Integer(int) => {
                                ret_asm = format!("\tli\ta0, {}\n\tret\n", int.value());
                            }
                            ValueKind::Binary(_) => {
                                ret_asm = format!(
                                    "\tmv\ta0, {}\n\tret\n",
                                    Reg::find_reg(&mut regs, ret.value().expect("")).unwrap()
                                );
                            }
                            _ => (),
                        }
                        asm.push_str(&ret_asm);
                    }
                    ValueKind::Binary(binary) => {
                        let mut ret_asm: String = String::new();
                        let mut lhs_reg: String = String::new();
                        let mut rhs_reg: String = String::new();
                        let self_reg: String = Reg::get_reg(&mut regs, inst).unwrap();
                        let lhs_data = self.dfg().value(binary.lhs());
                        let rhs_data = self.dfg().value(binary.rhs());
                        match lhs_data.kind() {
                            ValueKind::Integer(int) => {
                                if int.value() == 0 {
                                    lhs_reg = "x0".to_string();
                                } else {
                                    lhs_reg = Reg::get_reg(&mut regs, binary.lhs()).unwrap();
                                    ret_asm.push_str(&format!(
                                        "\tli\t{rd}, {imm}\n",
                                        rd = lhs_reg,
                                        imm = int.value()
                                    ));
                                }
                            }
                            ValueKind::Binary(_) => {
                                lhs_reg = Reg::find_reg(&mut regs, binary.lhs()).unwrap();
                            }
                            _ => (),
                        }
                        match rhs_data.kind() {
                            ValueKind::Integer(int) => {
                                if int.value() == 0 {
                                    rhs_reg = "x0".to_string();
                                } else {
                                    rhs_reg = Reg::get_reg(&mut regs, binary.lhs()).unwrap();
                                    ret_asm.push_str(&format!(
                                        "\tli\t{rd}, {imm}\n",
                                        rd = rhs_reg,
                                        imm = int.value()
                                    ));
                                }
                            }
                            ValueKind::Binary(_) => {
                                rhs_reg = Reg::find_reg(&mut regs, binary.rhs()).unwrap();
                            }
                            _ => (),
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
                            _ => (),
                        }
                        Reg::free_reg(&mut regs, binary.lhs());
                        Reg::free_reg(&mut regs, binary.rhs());
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
  .text         # 声明之后的数据需要被放入代码段中
  .globl main   # 声明全局符号 main, 以便链接器处理
main:           # 标记 main 的入口点
  li a0, 0      # 将整数 0 加载到存放返回值的 a0 寄存器中
  ret           # 返回
*/
