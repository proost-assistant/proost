use log::warn;

use crate::backend::payload::notification::Notification;
use crate::backend::LanguageServer;

pub struct Dispatcher<'a, T: LanguageServer> {
    notification: Option<Notification>,

    backend: &'a mut T,
}

impl<'a, T: LanguageServer> Dispatcher<'a, T> {
    pub fn new(notification: Notification, backend: &'a mut T) -> Self {
        Self {
            notification: Some(notification),

            backend,
        }
    }

    pub fn handle<N>(&mut self, closure: fn(&mut T, N::Params)) -> &mut Self
    where
        N: lsp_types::notification::Notification,
    {
        self.handle_callback::<N, _>(closure, || ())
    }

    pub fn handle_callback<N, C>(&mut self, handler: fn(&mut T, N::Params), callback: C) -> &mut Self
    where
        C: FnOnce() -> (),
        N: lsp_types::notification::Notification,
    {
        let Some(ref notification) = self.notification else { return self; };

        if notification.method != N::METHOD {
            return self;
        }

        let mut notification = self.notification.take().unwrap();

        let params = serde_json::from_value::<N::Params>(notification.params.take()).unwrap();

        handler(self.backend, params);

        callback();

        self
    }

    pub fn handle_fallthrough(&mut self, error_message: &str) {
        let Some(_) = self.notification else { return; };

        warn!("{error_message}");
    }
}
