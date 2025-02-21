use crate::ast::block::{BlockEnd, BlockGraph, BlockID, Branch, Jump};

/// First&Last Step: remove unused block
///
/// if the block has no previous block and it's not the entry,
/// then remove it
pub fn remove_unused_block(block_graph: &mut BlockGraph) {
    let mut remove: Vec<BlockID> = Vec::new();
    for id in block_graph.map_iter() {
        let block = block_graph.find_mut_block(id);
        if block.is_unused() && id != block_graph.get_entry() {
            remove.push(id);
        }
    }

    remove.iter().for_each(|id| {
        block_graph.unlink(*id);
    });
}

/// Step 3: remove empty block
///
/// Before this step, the ir should only have jump-end empty block
pub fn remove_empty_block(block_graph: &mut BlockGraph) {
    let mut remove: Vec<BlockID> = Vec::new();
    for id in block_graph.map_iter() {
        let block = block_graph.find_block(id).clone();
        if block.is_empty() {
            if let BlockEnd::Jump(_) = block.block_end() {
                remove.push(id);
            }
        }
    }

    remove.iter().for_each(|id| {
        block_graph.unlink(*id);
    });
}

/// Step 2: replace constant-val branch with jump
///
pub fn repl_const_val_br(block_graph: &mut BlockGraph) {
    for id in block_graph.map_iter() {
        let block = block_graph.find_block(id).clone();
        if let BlockEnd::Branch(Branch { cond, cons, alter }) = block.block_end() {
            if let Ok(cond) = cond.parse::<i32>() {
                let (t_id, f_id) = if cond != 0 {
                    (*cons, *alter)
                } else {
                    (*alter, *cons)
                };
                block_graph.find_mut_block(id).next.remove(&f_id);

                let f_id = block_graph.find_mut_block(f_id);
                f_id.prev.remove(&id);

                let jump = BlockEnd::Jump(Jump { dest: t_id });
                block_graph.repl_end(id, jump);
            };
        };
    }
}
