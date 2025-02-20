use std::collections::HashMap;

use crate::ast::block::{BlockEnd, BlockGraph, BlockID, Branch, Jump};

mod opt;

type NameMap = HashMap<BlockID, String>;

fn insert_block_name(block_graph: &mut BlockGraph, name_map: &mut NameMap) {
    let mut cnt: u32 = 0;
    for id in block_graph.iter() {
        let name: String = if id == block_graph.get_entry() {
            "entry".to_string()
        } else {
            cnt += 1;
            format!("BR{}", cnt)
        };

        let block = block_graph.find_mut_block(id);
        name_map.insert(id, name.clone());
        block.set_name(name);
    }
}

fn insert_block_end(block_graph: &mut BlockGraph, name_map: NameMap) {
    for id in block_graph.iter() {
        let block = block_graph.find_mut_block(id);
        match block.block_end() {
            BlockEnd::Ret => (),
            BlockEnd::Jump(Jump { dest }) => {
                let dest = name_map.get(dest).unwrap();
                block.insert_ir(format!("\tjump %{dest}\n"));
            }
            BlockEnd::Branch(Branch { cond, cons, alter }) => {
                let cons = name_map.get(cons).unwrap();
                let alter = name_map.get(alter).unwrap();
                block.insert_ir(format!("\tbr {cond}, %{cons}, %{alter}\n"));
            }
        }
    }
}

pub fn generate_ir(block_graph: &mut BlockGraph) -> String {
    let mut ir: String = String::new();
    let mut name_map: NameMap = HashMap::new();

    dbg!(&block_graph);
    opt::remove_unused_block(block_graph);
    opt::repl_const_val_br(block_graph);
    opt::remove_empty_block(block_graph);
    opt::remove_unused_block(block_graph);
    dbg!(&block_graph);
    insert_block_name(block_graph, &mut name_map);
    insert_block_end(block_graph, name_map);
    dbg!(&block_graph);

    for id in block_graph.iter() {
        let block = block_graph.find_block(id);
        ir += &block.generate_ir();
    }

    ir
}
