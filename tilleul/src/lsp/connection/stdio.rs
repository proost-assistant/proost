//! I/O communication via `stdio`.

use std::io::{BufReader, BufWriter};
use std::{io, thread};

use crossbeam_channel::{unbounded, Receiver, RecvError, SendError, Sender};
use log::{debug, error, info};

use super::thread::Threads;
use super::{LanguageServer, Server};
use crate::lsp::message::notification::Notification;
use crate::lsp::message::Message;

/// Message passing from the spawned threads.
///
/// Please note that the `Receiver` and `Sender` have unbounded capacity, because [`Server`] does not have a worker pool.
///
/// [`Server`]: crate::lsp::server::Server
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

        let reader = Some(thread::spawn(move || Self::reader_thread(&reader_sender, BufReader::new(io::stdin()))));
        let writer = Some(thread::spawn(move || Self::writer_thread(&writer_receiver, BufWriter::new(io::stdout()))));

        Self {
            receiver,
            sender,
            _threads: Threads { reader, writer },
        }
    }

    /// Reader thread function.
    #[no_coverage]
    fn reader_thread<R: std::io::Read>(sender: &Sender<Message>, mut reader: BufReader<R>) {
        info!("Reader thread started");

        loop {
            let msg = Message::read(&mut reader);

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
    #[no_coverage]
    fn writer_thread<W: std::io::Write>(receiver: &Receiver<Message>, mut writer: BufWriter<W>) {
        info!("Writer thread started");

        for msg in receiver {
            debug!("Sending: {:?}", msg);

            msg.write(&mut writer).unwrap_or_else(|err| {
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

#[cfg(test)]
mod tests {
    use lsp_types::InitializedParams;

    use super::*;
    use crate::lsp::message::notification::Notification;
    use crate::lsp::message::response::Response;
    use crate::lsp::message::Message;

    #[test]
    fn reader_got_message() {
        let data = b"Content-Length: 41\r\n\r\n{ \"method\": \"initialized\", \"params\": {} }";

        let (reader_sender, receiver) = unbounded::<Message>();

        let thread = Some(thread::spawn(move || {
            let reader = BufReader::new(&data[..]);

            Stdio::reader_thread(&reader_sender, reader);
        }));

        let msg = receiver.recv().unwrap();

        assert_eq!(msg, Message::Notification(Notification::new::<lsp_types::notification::Initialized>(InitializedParams {})));
        assert!(thread.unwrap().join().is_ok());
    }

    #[test]
    fn reader_exiting() {
        let data = b"";

        let (reader_sender, receiver) = unbounded::<Message>();

        let thread = Some(thread::spawn(move || {
            let reader = BufReader::new(&data[..]);

            Stdio::reader_thread(&reader_sender, reader);
        }));

        assert_eq!(receiver.recv().unwrap_err(), RecvError);
        assert!(thread.unwrap().join().is_ok());
    }

    #[test]
    fn reader_got_corrupted_message() {
        let data = b"{ \"method\": \"initialized\", \"params\": {} }";

        let (reader_sender, receiver) = unbounded::<Message>();

        let thread = Some(thread::spawn(move || {
            let reader = BufReader::new(&data[..]);

            Stdio::reader_thread(&reader_sender, reader);
        }));

        assert_eq!(receiver.recv().unwrap_err(), RecvError);
        assert!(thread.unwrap().join().is_ok());
    }

    #[test]
    #[allow(unused_assignments)]
    fn writer_send_message() {
        let data = Message::Response(Response::new::<lsp_types::request::Shutdown>(1, ()));
        let mut thread = None;

        {
            let (sender, writer_receiver) = unbounded::<Message>();

            thread = Some(thread::spawn(move || {
                let writer = BufWriter::new(Vec::new());

                Stdio::writer_thread(&writer_receiver, writer);
            }));

            assert_eq!(sender.send(data), Ok(()));
        };

        assert!(thread.unwrap().join().is_ok());
    }
}
