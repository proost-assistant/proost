use kernel::memory::arena::Arena;

use crate::backend::connection::Connection;

pub mod handler;

pub struct Tilleul<'a, 'arena> {
    arena: &'a mut Arena<'arena>,
    connection: &'a Connection,
}

impl<'a, 'arena> Tilleul<'a, 'arena> {
    pub fn new(arena: &'a mut Arena<'arena>, connection: &'a Connection) -> Self {
        Self { arena, connection }
    }
}
