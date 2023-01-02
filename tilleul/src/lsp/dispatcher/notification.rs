//! [`Notification`] dispatcher.

use log::warn;

use crate::lsp::message::notification::Notification;
use crate::lsp::LanguageServer;

/// Dispatches [`Notification`] to [`LanguageServer`].
///
/// The [`Dispatcher`] has to be called through every supported [`handle`]-like method, according to the [`State`] of [`Server`].
///
/// Each [`handle`]-like method will look at the method of [`Notification`]. If there is a match, the corresponding
/// [`LanguageServer`]'s method is called with according JSON's serialisation and deserialisation.
///
/// If there is no match after every [`handle`]-like functions, the `handle_fallthrough` method should be called.
///
/// [`handle`]: (Dispatcher::handle)
/// [`State`]: (super::server::State)
/// [`Server`]: (super::server::Server)
pub(in crate::lsp) struct Dispatcher<'dispatcher, T: LanguageServer> {
    /// [`Notification`] to be dispatched.
    ///
    /// Will be transformed into `None` if consumed.
    notification: Option<Notification>,

    /// [`LanguageServer`] where the [`Notification`] is dispatched.
    backend: &'dispatcher mut T,
}

impl<'dispatcher, T: LanguageServer> Dispatcher<'dispatcher, T> {
    /// Creates a new [`Dispatcher`].
    pub fn new(notification: Notification, backend: &'dispatcher mut T) -> Self {
        Self {
            notification: Some(notification),

            backend,
        }
    }

    /// Dispatches the [`Request`] to the [`LanguageServer`], if the [`Notification`]'s method correspond to the
    /// [`lsp_types::notification::Notification::METHOD`].
    ///
    /// [`Request`]: (crate::lsp::payload::request::Request)
    pub fn handle<N>(&mut self, closure: fn(&mut T, N::Params)) -> &mut Self
    where
        N: lsp_types::notification::Notification,
    {
        self.handle_callback::<N, _>(closure, || ())
    }

    /// Like [`handle`], but also accepts a callback to be executed after the request has been handled.
    ///
    /// [`handle`]: (Dispatcher::handle)
    pub fn handle_callback<N, C>(&mut self, handler: fn(&mut T, N::Params), callback: C) -> &mut Self
    where
        C: FnOnce(),
        N: lsp_types::notification::Notification,
    {
        let Some(ref notification) = self.notification else { return self; };

        if notification.method != N::METHOD {
            return self;
        }

        let mut notification = self.notification.take().unwrap_or_else(|| unreachable!("checked as Some above"));

        let params = serde_json::from_value::<N::Params>(notification.params.take())
            .unwrap_or_else(|_| unreachable!("must be run only once, as notification.take() is performed above"));

        handler(self.backend, params);

        callback();

        self
    }

    /// Fallthrough handler if the [`Notification`] is not handled by any [`handle`]-like methods.
    ///
    /// This function should be used at the end of the [`handle`]-like methods chain.
    ///
    /// [`handle`]: (Dispatcher::handle)
    pub fn handle_fallthrough(&mut self, error_message: &str) {
        let Some(_) = self.notification else { return; };

        warn!("{error_message}");
    }
}
