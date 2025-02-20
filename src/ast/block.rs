use std::collections::{HashMap, HashSet, VecDeque};

pub type BlockID = u32;

#[derive(Debug, Clone)]
pub struct Branch {
    pub cond: String,
    pub cons: BlockID,
    pub alter: BlockID,
}

#[derive(Debug, Clone)]
pub struct Jump {
    pub dest: BlockID,
}

#[derive(Debug, Clone)]
pub enum BlockEnd {
    Ret,
    Branch(Branch),
    Jump(Jump),
}

#[derive(Debug, Clone)]
pub struct BaseBlock {
    name: String,
    ir: String,
    block_end: Option<BlockEnd>,
    pub prev: HashSet<BlockID>,
    pub next: Vec<BlockID>,
}
// ir bond base_block is a good idea ithk

impl BaseBlock {
    fn new(name: String) -> Self {
        BaseBlock {
            name,
            block_end: None,
            ir: String::new(),
            prev: HashSet::new(),
            next: Vec::new(),
        }
    }

    pub fn set_name(&mut self, name: String) {
        self.name = name;
    }

    pub fn insert_ir(&mut self, ir: String) {
        self.ir += &ir;
    }

    pub fn is_empty(&self) -> bool {
        self.ir.is_empty()
    }

    pub fn is_unused(&self) -> bool {
        self.prev.is_empty()
    }

    pub fn block_end(&self) -> &BlockEnd {
        self.block_end.as_ref().unwrap()
    }

    pub fn replace(&mut self, id1: BlockID, id2: BlockID) {
        match self.block_end.as_mut().unwrap() {
            BlockEnd::Ret => (),
            BlockEnd::Branch(Branch {
                cond: _,
                cons,
                alter,
            }) => {
                *cons = if *cons == id1 { id2 } else { *cons };
                *alter = if *alter == id1 { id2 } else { *alter };
            }
            BlockEnd::Jump(Jump { dest }) => {
                if *dest == id1 {
                    *dest = id2
                }
            }
        };

        self.next.iter_mut().for_each(|x| {
            if *x == id1 {
                *x = id2;
            }
        });
    }

    pub fn generate_ir(&self) -> String {
        format!("%{}:\n{}", self.name, self.ir)
    }
}

pub const DEFAULT_ENTRY: BlockID = 1;
pub const INVALID: BlockID = 0;

#[derive(Debug)]
pub struct BlockGraph {
    map: HashMap<BlockID, BaseBlock>,
    curr_id: BlockID,
    block_id_cnt: u32,
    entry: BlockID,
}

impl BlockGraph {
    pub fn new() -> Self {
        BlockGraph {
            map: HashMap::new(),
            curr_id: INVALID,
            block_id_cnt: DEFAULT_ENTRY,
            entry: DEFAULT_ENTRY,
        }
    }

    /// default mut
    /// return a mutable block
    fn curr_block(&mut self) -> &mut BaseBlock {
        self.find_mut_block(self.curr_id)
    }

    pub fn find_mut_block(&mut self, id: BlockID) -> &mut BaseBlock {
        match self.map.get_mut(&id) {
            Some(base_block) => base_block,
            None => panic!("can't find {id}: the map is empty!"),
        }
    }

    pub fn find_block(&self, id: BlockID) -> &BaseBlock {
        match self.map.get(&id) {
            Some(base_block) => base_block,
            None => panic!("can't find {id}: the map is empty!"),
        }
    }

    pub fn get_entry(&self) -> BlockID {
        self.entry
    }

    pub fn set_corrent_id(&mut self, id: BlockID) {
        self.curr_id = id;
    }

    pub fn get_new_block(&mut self) -> BlockID {
        let id = self.block_id_cnt;
        self.map.insert(id, BaseBlock::new(String::new()));
        self.block_id_cnt += 1;
        id
    }

    /// Unlink basic block will remove the block with block id @id
    /// then connect the previous block and next block
    /// Only allow this two type:
    ///     1. jump-end block
    ///         If the jump-end block is entry block, the next block
    ///         will become the new entry
    ///     2. unused block
    ///         Unlink will remove unused block recursively
    ///eg:
    ///   |-----------------|
    /// prev ---/--|        |
    ///            v        v
    /// prev -/-> id -/-> next
    /// ^                  ^
    /// |------------------|
    ///
    #[inline]
    pub fn unlink(&mut self, id: BlockID) {
        let block = self.find_block(id).clone();
        dbg!(&block);

        if block.prev.is_empty() && id != self.entry {
            for next in block.next {
                let next_block = self.find_mut_block(next);
                next_block.prev.remove(&id);
                if next_block.prev.is_empty() {
                    self.unlink(next);
                }
            }
            self.map.remove(&id);
            return;
        }

        if let BlockEnd::Jump(Jump { dest }) = block.block_end.unwrap() {
            let next = self.find_mut_block(dest);
            next.prev.remove(&id);

            if id == self.entry {
                self.entry = dest;
                self.map.remove(&id);
                return;
            }

            for prev in block.prev {
                let next = self.find_mut_block(dest);
                next.prev.insert(prev);
                let prev = self.find_mut_block(prev);
                prev.replace(id, dest);
            }
        } else {
            panic!("Can't unlink no-jump-end block!");
        }
        self.map.remove(&id);
    }

    pub fn repl_end(&mut self, id: BlockID, end: BlockEnd) {
        self.find_mut_block(id).block_end = Some(end);
    }

    pub fn start(&mut self) {
        self.set_corrent_id(DEFAULT_ENTRY);
        self.get_new_block();
    }

    pub fn end(&mut self, block_end: BlockEnd) {
        if self.curr_block().block_end.is_some() {
            return;
        }

        let (mut switch, mut switch_id): (bool, BlockID) = (false, INVALID);

        match block_end {
            BlockEnd::Ret => (),
            BlockEnd::Jump(Jump { dest }) => self.insert_next(dest),
            BlockEnd::Branch(Branch {
                cond: _,
                cons,
                alter,
            }) => {
                self.insert_next(cons);
                self.insert_next(alter);
                (switch, switch_id) = (true, cons);
            }
        };

        self.curr_block().block_end = Some(block_end);

        if switch {
            self.set_corrent_id(switch_id);
        }
    }

    /// push next into current block
    /// also push prev into the next block
    pub fn insert_next(&mut self, next: BlockID) {
        self.curr_block().next.push(next);
        let curr_id = self.curr_id;
        self.find_mut_block(next).prev.insert(curr_id);
    }

    pub fn insert_ir(&mut self, ir: &str) {
        if self.curr_block().block_end.is_none() {
            self.curr_block().ir += ir;
        }
    }

    pub fn iter(&self) -> BlockIter {
        BlockIter::new(self.map.clone(), self.entry)
    }
    pub fn map_iter(&self) -> std::collections::hash_map::IntoKeys<u32, BaseBlock> {
        self.map.clone().into_keys()
    }
}

pub struct BlockIter {
    queue: VecDeque<BlockID>,
    map: HashMap<BlockID, BaseBlock>,
    set: HashSet<BlockID>,
}

impl BlockIter {
    fn new(map: HashMap<BlockID, BaseBlock>, entry: BlockID) -> BlockIter {
        BlockIter {
            queue: VecDeque::from([entry]),
            map,
            set: HashSet::from([entry]),
        }
    }
}

impl Iterator for BlockIter {
    type Item = BlockID;

    fn next(&mut self) -> Option<Self::Item> {
        if self.queue.is_empty() {
            return None;
        }

        let id = self.queue.pop_front().unwrap();
        for ele in self.map.get(&id).unwrap().next.clone() {
            if !self.set.contains(&ele) {
                self.set.insert(ele);
                self.queue.push_back(ele);
            }
        }
        Some(id)
    }
}
