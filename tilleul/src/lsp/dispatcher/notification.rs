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
pub(in crate::lsp) struct Dispatcher<'dispatcher, S: LanguageServer> {
    /// [`Notification`] to be dispatched.
    ///
    /// Will be transformed into `None` if consumed.
    notification: Option<Notification>,

    /// [`LanguageServer`] where the [`Notification`] is dispatched.
    backend: &'dispatcher mut S,
}

impl<'dispatcher, S: LanguageServer> Dispatcher<'dispatcher, S> {
    /// Creates a new [`Dispatcher`].
    pub fn new(notification: Notification, backend: &'dispatcher mut S) -> Self {
        Self {
            notification: Some(notification),

            backend,
        }
    }

    /// Dispatches the [`Notification`] to the [`LanguageServer`], if the associated method corresponds to
    /// [`lsp_types::notification::Notification::METHOD`].
    #[no_coverage]
    pub fn handle<N>(&mut self, closure: fn(&mut S, N::Params)) -> &mut Self
    where
        N: lsp_types::notification::Notification,
    {
        self.handle_callback::<N, _>(closure, || ())
    }

    /// Like [`handle`], but also accepts a callback to be executed after the request has been handled.
    ///
    /// [`handle`]: (Dispatcher::handle)
    #[no_coverage]
    pub fn handle_callback<N, C>(&mut self, handler: fn(&mut S, N::Params), callback: C) -> &mut Self
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
            .unwrap_or_else(|_| unreachable!("may only be run once, as notification.take() is performed above"));

        handler(self.backend, params);

        callback();

        self
    }

    /// Fallthrough handler if the [`Notification`] is not handled by any [`handle`]-like methods.
    ///
    /// This function should be used at the end of the [`handle`]-like methods chain.
    ///
    /// [`handle`]: (Dispatcher::handle)
    #[no_coverage]
    pub fn handle_fallthrough(&mut self, error_message: &str) {
        let Some(ref notification) = self.notification else { return; };

        warn!("{error_message} on {}", notification.method);
    }
}

#[cfg(test)]
mod tests {
    use lsp_types::notification::{DidOpenTextDocument, Initialized};
    use lsp_types::{DidOpenTextDocumentParams, InitializedParams};
    use mockall::predicate::eq;

    use super::*;
    use crate::lsp::MockLanguageServer;

    fn mock_initialized(mock: &mut MockLanguageServer, params: InitializedParams) {
        mock.initialized(params);
    }

    fn mock_text_document_did_open(mock: &mut MockLanguageServer, params: DidOpenTextDocumentParams) {
        mock.text_document_did_open(params);
    }

    #[test]
    fn dispatcher_not_handled() {
        let mut backend = MockLanguageServer::new();

        Dispatcher::new(Notification::new::<Initialized>(InitializedParams {}), &mut backend).handle_fallthrough("fallthrough");
    }

    #[test]
    fn dispatcher_handled() {
        let mut backend = MockLanguageServer::new();

        backend.expect_initialized().with(eq(InitializedParams {})).times(2).return_const(());

        Dispatcher::new(Notification::new::<Initialized>(InitializedParams {}), &mut backend)
            .handle::<Initialized>(mock_initialized)
            .handle::<DidOpenTextDocument>(mock_text_document_did_open)
            .handle_fallthrough("fallthrough");

        Dispatcher::new(Notification::new::<Initialized>(InitializedParams {}), &mut backend)
            .handle::<DidOpenTextDocument>(mock_text_document_did_open)
            .handle::<Initialized>(mock_initialized)
            .handle_fallthrough("fallthrough");
    }
}
