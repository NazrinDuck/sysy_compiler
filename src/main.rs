use asm::{DumpAsm, RegAllocator};
use ast::{block::BlockGraph, symbol::SymTable, CompUnit, DumpIR};
use ir::generate_ir;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{read_to_string, File};
use std::io::Write;

mod asm;
mod ast;
mod ir;

lalrpop_mod!(sys);

fn main() -> std::io::Result<()> {
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    let source_code = read_to_string(input)?;
    let out_file = File::create(output)?;

    let ast = sys::CompUnitParser::new().parse(&source_code).unwrap();
    //println!("{:#?}", ast);
    let ast = generate_ast(ast);

    println!("\x1b[01;32mast:\n{}\x1b[0m", ast);

    match &mode[..] {
        "-koopa" => generate_file(out_file, ast.clone()).unwrap(),
        "-riscv" => {
            let driver = koopa::front::Driver::from(ast.clone());
            let program = driver.generate_program().unwrap();

            let asm: String = program.dump_asm(&mut RegAllocator::new());

            generate_file(out_file, asm.clone()).unwrap();
            println!("\x1b[01;36masm:\n{}\x1b[0m", asm);
        }
        _ => (),
    }

    // let a: ast::UnaryExp = ast::UnaryExp::PrimaryExp(Box::new(ast::PrimaryExp::Number(12)));

    Ok(())
}

fn generate_ast(comp_unit: CompUnit) -> String {
    let mut sym_table = SymTable::new();
    let mut block_graph = BlockGraph::new();
    let ast = comp_unit.dump_ir(&mut sym_table, &mut block_graph);
    //print!("{}", generate_ir(&mut block_graph));
    // dbg!(sym_table);
    // block_graph.generate_ir();
    ast
}

fn generate_file(mut file: File, content: String) -> Result<(), Box<dyn std::error::Error>> {
    file.write_all(content.as_bytes())?;
    Ok(())
}
