use crate::message::Message;
use crossbeam_channel::{bounded, Receiver, Sender};
use log::{error, info};
use std::{io, thread};

pub struct Connection {
    pub sender: Sender<Message>,
    pub receiver: Receiver<Message>,
}

pub struct Threads {
    reader: thread::JoinHandle<()>,
    writer: thread::JoinHandle<()>,
}

impl Connection {
    /// Create a new connection from a mode.
    pub fn new() -> (Connection, Threads) {
        let (reader_sender, receiver) = bounded::<Message>(0);
        let (sender, writer_receiver) = bounded::<Message>(0);

        let reader = thread::spawn(|| Connection::reader_thread(reader_sender));
        let writer = thread::spawn(|| Connection::writer_thread(writer_receiver));

        (Connection { sender, receiver }, Threads { reader, writer })
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

impl Threads {
    pub fn join(self) {
        self.reader.join().unwrap();
        self.writer.join().unwrap();
    }
}
