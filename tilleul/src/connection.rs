use crate::message::Message;
use clap::Subcommand;
use crossbeam_channel::{bounded, Receiver, Sender};
use log::{error, info};
use std::{io, thread};

pub struct Connection {
    pub sender: Sender<Message>,
    pub receiver: Receiver<Message>,
}

#[derive(Clone, Debug, Subcommand)]
pub enum ConnectionMode {
    Stdio,
    Pipe { file: String },
    Socket { port: u16 },
}

pub struct Threads {
    reader: thread::JoinHandle<io::Result<()>>,
    writer: thread::JoinHandle<io::Result<()>>,
}

impl Connection {
    pub fn new(_mode: Option<ConnectionMode>) -> (Connection, Threads) {
        let (reader_sender, receiver) = bounded::<Message>(0);
        let (sender, writer_receiver) = bounded::<Message>(0);

        let reader = thread::spawn(|| Connection::reader_thread(reader_sender));
        let writer = thread::spawn(|| Connection::writer_thread(writer_receiver));

        (Connection { sender, receiver }, Threads { reader, writer })
    }

    fn reader_thread(sender: Sender<Message>) -> io::Result<()> {
        let mut stdin = io::stdin().lock();

        loop {
            let msg = match Message::read(&mut stdin) {
                Ok(msg) => msg,
                Err(err) => {
                    error!("{:?}", err);
                    continue;
                }
            };

            info!("Got: {:?}", msg);

            sender.send(msg).unwrap();
        }
    }

    fn writer_thread(receiver: Receiver<Message>) -> io::Result<()> {
        let mut stdout = io::stdout().lock();

        receiver
            .into_iter()
            .try_for_each(|it| it.write(&mut stdout))
    }
}

impl Threads {
    pub fn join(self) -> io::Result<()> {
        self.reader.join().unwrap()?;
        self.writer.join().unwrap()
    }
}
