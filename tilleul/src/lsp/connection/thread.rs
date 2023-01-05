//! Threads wrapper utility.

use std::thread;

use log::error;

/// Spawned threads helper for I/O communication.
pub(super) struct Threads {
    /// Reader thread.
    pub reader: Option<thread::JoinHandle<()>>,

    /// Writer thread.
    pub writer: Option<thread::JoinHandle<()>>,
}

impl Drop for Threads {
    fn drop(&mut self) {
        if let Some(thread) = self.reader.take() {
            thread.join().unwrap_or_else(|_| error!("Failed to join reader thread"));
        }

        if let Some(thread) = self.writer.take() {
            thread.join().unwrap_or_else(|_| error!("Failed to join writer thread"));
        }
    }
}
