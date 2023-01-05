//! Input/Output communication.
//!
//! I/O is performed with two threads, one for reading and one for writing.
//! Using two threads for I/O is a common pattern to avoid using non-blocking polling.
//!
//! [`crossbeam-channel`] is used to communicate between the threads, using message passing.

use crossbeam_channel::{RecvError, SendError};

use crate::lsp::message::notification::Notification;
use crate::lsp::message::Message;

pub mod stdio;
pub mod thread;

/// A trait defining a communication channel for the [`LanguageServer`].
///
/// This allows only to send [`Notification`]s to the client.
///
/// [`LanguageServer`]: crate::lsp::LanguageServer
#[cfg_attr(test, mockall::automock)]
pub trait LanguageServer {
    /// Sends a [`Notification`] to the client.
    fn send(&self, notification: Notification);
}

/// A trait defining a communication channel for the [`Server`].
///
/// [`Server`]: crate::lsp::Server
#[cfg_attr(test, mockall::automock)]
pub trait Server {
    /// Reads a [`Message`] from the client.
    ///
    /// # Errors
    /// Returns an error if the channel is disconnected.
    fn receive(&self) -> Result<Message, RecvError>;

    /// Sends a [`Message`] to the client.
    ///
    /// # Errors
    /// Returns an error if the channel is disconnected.
    fn send(&self, message: Message) -> Result<(), SendError<Message>>;
}
