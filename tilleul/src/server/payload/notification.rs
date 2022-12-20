use lsp_types::notification;
use serde::{Deserialize, Serialize};
use serde_json::Value;

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
    /// Creates a new notification.
    pub fn new<N: notification::Notification>(params: N::Params) -> Self {
        Self {
            method: N::METHOD.to_string(),
            params: serde_json::to_value(params).unwrap(),
        }
    }

    pub fn is_exit(&self) -> bool {
        use notification::Notification;

        self.method == *notification::Exit::METHOD
    }
}
