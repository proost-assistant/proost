use crate::payload::Message;
use crossbeam_channel::{bounded, Receiver, Sender};
use log::{error, info};
use std::{io, thread};

pub struct Connection {
    pub sender: Sender<Message>,
    pub receiver: Receiver<Message>,

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

    fn reader_thread(sender: Sender<Message>) {
        let mut stdin = io::stdin().lock();

        info!("Reader thread started");

        loop {
            match Message::read(&mut stdin) {
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

        receiver.into_iter().for_each(|msg| msg.write(&mut stdout));

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
