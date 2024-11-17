use asm::DumpAsm;
use ast::DumpIR;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{read_to_string, File};
use std::io::Write;

mod asm;
mod ast;
// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sys);

fn main() -> std::io::Result<()> {
    // 解析命令行参数
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    // 读取输入文件
    let source_code = read_to_string(input)?;
    let out_file = File::create(output)?;

    let ast = sys::CompUnitParser::new().parse(&source_code).unwrap();
    println!("{:#?}", ast);
    let ast = ast.dump_ir();
    println!("\x1b[01;32mast:\n{}\x1b[0m", ast);

    match &mode[..] {
        "-koopa" => generate_file(out_file, ast.clone()).unwrap(),
        "-riscv" => {
            let driver = koopa::front::Driver::from(ast.clone());
            let program = driver.generate_program().unwrap();
            let asm: String = program.dump_asm();

            generate_file(out_file, asm.clone()).unwrap();
            println!("\x1b[01;36masm:\n{}\x1b[0m", asm);
        }
        _ => (),
    }

    // let a: ast::UnaryExp = ast::UnaryExp::PrimaryExp(Box::new(ast::PrimaryExp::Number(12)));
    // 调用 lalrpop 生成的 parser 解析输入文件

    //parser(program);

    Ok(())
}

fn generate_file(mut file: File, content: String) -> Result<(), Box<dyn std::error::Error>> {
    file.write_all(content.as_bytes())?;
    Ok(())
}
