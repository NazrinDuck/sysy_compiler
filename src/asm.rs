use koopa::ir::{entities::ValueData, Program, ValueKind};

pub trait DumpAsm {
    fn dump_asm(&self) -> String;
}

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
        let mut asm: String = String::new();
        for (&bb, node) in self.layout().bbs() {
            let basic_block_data = self.dfg().bb(bb);
            println!("{:#?}", basic_block_data.name());
            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);
                match value_data.kind() {
                    ValueKind::Integer(int) => {
                        dbg!(int);
                    }

                    ValueKind::Return(ret) => {
                        let mut ret_asm: String = String::new();
                        let value_data = self.dfg().value(ret.value().expect(""));
                        match value_data.kind() {
                            ValueKind::Integer(int) => {
                                ret_asm = format!("\tli a0, {}\n\tret\n", int.value());
                            }
                            _ => unreachable!(),
                        }
                        asm.push_str(&ret_asm);
                        dbg!(self.dfg().value(ret.value().expect("")));
                    }
                    // 其他种类暂时遇不到
                    _ => unreachable!(),
                }
            }
        }
        asm
    }
}

/*
impl DumpAsm for ValueData {
    fn dump_asm(&self) -> String {
        let mut asm: String = String::new();
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
