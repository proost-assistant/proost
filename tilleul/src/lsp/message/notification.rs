//! A [notification] sent from a client to a server, or from a server to a client.
//!
//! [notification]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage

use lsp_types::notification;
use serde::{Deserialize, Serialize};
use serde_json::Value;

/// [Notification] message.
///
/// [Notification]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Notification {
    /// The method to be invoked.
    pub method: String,

    /// The notification's params.
    #[serde(default)]
    #[serde(skip_serializing_if = "Value::is_null")]
    pub params: Value,
}

impl Notification {
    /// Creates a new [`Notification`].
    pub fn new<N: notification::Notification>(params: N::Params) -> Self {
        Self {
            method: N::METHOD.to_owned(),
            params: serde_json::to_value(params).unwrap_or_else(|_| unreachable!("lsp_types crate is assumed to be correct")),
        }
    }

    /// Returns true if the notification is the [`exit`] notification.
    ///
    /// [`exit`]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#exit
    #[must_use]
    pub fn is_exit(&self) -> bool {
        use notification::Notification;

        self.method == *notification::Exit::METHOD
    }
}
