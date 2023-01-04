//! I/O communication via `stdio`.

use std::{io, thread};

use crossbeam_channel::{unbounded, Receiver, RecvError, SendError, Sender};
use log::{debug, error, info};

use super::thread::Threads;
use super::{LanguageServer, Server};
use crate::lsp::message::notification::Notification;
use crate::lsp::message::Message;

/// Message passing from the spawned threads.
///
/// Please note that the `Receiver` and `Sender` have unbounded capacity, because [`Server`](super::server::Server) does not
/// have a worker pool.
pub struct Stdio {
    /// Read from the reader thread.
    pub(crate) receiver: Receiver<Message>,

    /// Write to the writer thread.
    pub(crate) sender: Sender<Message>,

    /// Spawned threads.
    _threads: Threads,
}

impl Stdio {
    /// Create a new `stdio` communication channel.
    #[must_use]
    pub fn new() -> Self {
        let (reader_sender, receiver) = unbounded::<Message>();
        let (sender, writer_receiver) = unbounded::<Message>();

        let reader = Some(thread::spawn(move || Self::reader_thread(&reader_sender)));
        let writer = Some(thread::spawn(move || Self::writer_thread(&writer_receiver)));

        Self {
            receiver,
            sender,
            _threads: Threads { reader, writer },
        }
    }

    /// Reader thread function.
    fn reader_thread(sender: &Sender<Message>) {
        let mut stdin = io::stdin().lock();

        info!("Reader thread started");

        loop {
            let msg = Message::read(&mut stdin);

            debug!("Received: {:?}", msg);

            match msg {
                Ok(Message::Notification(msg)) if msg.is_exit() => break,
                Ok(msg) => sender.send(msg).unwrap_or_else(|err| {
                    error!("Failed to send message to writer thread: {err}");
                }),
                Err(err) => error!("{:?}", err),
            };
        }

        info!("Reader thread exited");
    }

    /// Writer thread function.
    fn writer_thread(receiver: &Receiver<Message>) {
        let mut stdout = io::stdout().lock();

        info!("Writer thread started");

        for msg in receiver {
            debug!("Sending: {:?}", msg);

            msg.write(&mut stdout).unwrap_or_else(|err| {
                error!("Failed to write message to stdout: {err}");
            });
        }

        info!("Writer thread exited");
    }
}

impl Default for Stdio {
    fn default() -> Self {
        Self::new()
    }
}

impl LanguageServer for Stdio {
    fn send(&self, notification: Notification) {
        self.sender.send(Message::Notification(notification)).unwrap_or_else(|err| {
            error!("Failed to send message to writer thread: {err}");
        });
    }
}

impl Server for Stdio {
    fn receive(&self) -> Result<Message, RecvError> {
        self.receiver.recv()
    }

    fn send(&self, message: Message) -> Result<(), SendError<Message>> {
        self.sender.send(message)
    }
}
