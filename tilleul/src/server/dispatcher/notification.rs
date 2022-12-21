use log::warn;

use crate::server::payload::notification::Notification;
use crate::server::LanguageServerBackend;

pub struct Dispatcher<'a, T: LanguageServerBackend> {
    notification: Option<Notification>,

    backend: &'a mut T,
}

impl<'a, T: LanguageServerBackend> Dispatcher<'a, T> {
    pub fn new(notification: Notification, backend: &'a mut T) -> Self {
        Self {
            notification: Some(notification),

            backend,
        }
    }

    pub fn handle<N>(&mut self, closure: fn(&T, N::Params)) -> &mut Self
    where
        N: lsp_types::notification::Notification,
    {
        let Some(ref notification) = self.notification else { return self; };

        if notification.method != N::METHOD {
            return self;
        }

        let mut notification = self.notification.take().unwrap();

        let params = serde_json::from_value::<N::Params>(notification.params.take()).unwrap();

        closure(self.backend, params);

        self
    }

    pub fn handle_fallthrough(&mut self) {
        let Some(ref notification) = self.notification else { return; };

        warn!("Unknown {} notification received", notification.method);
    }
}
