//! A [notification] sent from a client to a server, or from a server to a client.
//!
//! [notification]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#notificationMessage

use serde::{Deserialize, Serialize};
use serde_json::Value;

/// [Notification] message.
///
/// [Notification]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#notificationMessage
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Notification {
    /// The method to be invoked.
    pub method: String,

    /// The notification parameters.
    #[serde(default)]
    #[serde(skip_serializing_if = "Value::is_null")]
    pub params: Value,
}

impl Notification {
    /// Creates a new [`Notification`].
    pub fn new<N: lsp_types::notification::Notification>(params: N::Params) -> Self {
        Self {
            method: N::METHOD.to_owned(),
            params: serde_json::to_value(params).unwrap_or_else(|_| unreachable!("lsp_types crate is assumed to be correct")),
        }
    }

    /// Indicates whether the notification is the [`exit`] notification.
    ///
    /// [`exit`]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#exit
    #[must_use]
    pub fn is_exit(&self) -> bool {
        use lsp_types::notification::Notification;

        self.method == *lsp_types::notification::Exit::METHOD
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_exit() {
        let cancel = Notification::new::<lsp_types::notification::Cancel>(lsp_types::CancelParams {
            id: lsp_types::NumberOrString::Number(0),
        });
        assert!(!cancel.is_exit());

        let exit = Notification::new::<lsp_types::notification::Exit>(());
        assert!(exit.is_exit());
    }
}
