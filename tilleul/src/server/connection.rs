use std::{io, thread};

use crossbeam_channel::{bounded, Receiver, Sender};
use log::{debug, error, info};

use super::payload::message::Message;

pub struct Connection {
    pub(crate) sender: Sender<Message>,
    pub(crate) receiver: Receiver<Message>,

    _threads: Threads,
}

struct Threads {
    reader: Option<thread::JoinHandle<()>>,
    writer: Option<thread::JoinHandle<()>>,
}

impl Connection {
    /// Create a new connection from a mode.
    pub fn new() -> Self {
        let (reader_sender, receiver) = bounded::<Message>(0);
        let (sender, writer_receiver) = bounded::<Message>(0);

        let reader = Some(thread::spawn(|| Connection::reader_thread(reader_sender)));
        let writer = Some(thread::spawn(|| Connection::writer_thread(writer_receiver)));

        Connection {
            sender,
            receiver,
            _threads: Threads { reader, writer },
        }
    }

    pub fn send(&self, message: Message) {
        self.sender.send(message).unwrap();
    }

    fn reader_thread(sender: Sender<Message>) {
        let mut stdin = io::stdin().lock();

        info!("Reader thread started");

        loop {
            let msg = Message::read(&mut stdin);

            debug!("Received: {:?}", msg);

            match msg {
                Ok(Message::Notification(msg)) if msg.is_exit() => break,
                Ok(msg) => sender.send(msg).unwrap(),
                Err(err) => error!("{:?}", err),
            };
        }

        info!("Reader thread exited");
    }

    fn writer_thread(receiver: Receiver<Message>) {
        let mut stdout = io::stdout().lock();

        info!("Writer thread started");

        for msg in receiver {
            debug!("Sending: {:?}", msg);

            msg.write(&mut stdout);
        }

        info!("Writer thread exited");
    }
}

impl Drop for Threads {
    fn drop(&mut self) {
        if let Some(thread) = self.reader.take() {
            thread.join().unwrap();
        }

        if let Some(thread) = self.writer.take() {
            thread.join().unwrap();
        }
    }
}
