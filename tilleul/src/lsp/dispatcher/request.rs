//! [`Request`] dispatcher.

use log::{error, warn};

use crate::lsp::message::request::Request;
use crate::lsp::message::response::{Error, Response};
use crate::lsp::message::Message;
use crate::lsp::{connection, LanguageServer};

/// Dispatches [`Request`] to [`LanguageServer`].
///
/// The [`Dispatcher`] has to be called through every supported [`handle`]-like method, according to the [`State`] of [`Server`].
///
/// Each [`handle`]-like method will look at the method of [`Request`]. If there is a match, the corresponding [`LanguageServer`]'s
/// method is called with according JSON's serialisation and deserialisation.
///
/// If there is no match after every [`handle`]-like functions, the `handle_fallthrough` method should be called.
///
/// [`handle`]: (Dispatcher::handle)
/// [`State`]: (super::server::State)
/// [`Server`]: (super::server::Server)
pub(in crate::lsp) struct Dispatcher<'dispatcher, S: LanguageServer, C: connection::Server> {
    /// [`Request`] to be dispatched.
    ///
    /// Will be transformed into `None` if consumed.
    request: Option<Request>,

    /// [`LanguageServer`] where the [`Request`] is dispatched.
    backend: &'dispatcher mut S,

    /// The [connection] to send [`Response`] to.
    ///
    /// [connection]: connection::Server
    connection: &'dispatcher C,
}

impl<'dispatcher, S: LanguageServer, C: connection::Server> Dispatcher<'dispatcher, S, C> {
    /// Creates a new [`Dispatcher`].
    pub fn new(request: Request, backend: &'dispatcher mut S, connection: &'dispatcher C) -> Self {
        Self {
            request: Some(request),

            backend,
            connection,
        }
    }

    /// Dispatches the [`Request`] to the [`LanguageServer`], if the [`Request`]'s method correspond to the
    /// [`lsp_types::request::Request::METHOD`].
    #[allow(dead_code)]
    pub fn handle<R>(&mut self, closure: fn(&mut S, R::Params) -> R::Result) -> &mut Self
    where
        R: lsp_types::request::Request,
    {
        self.handle_callback::<R, _>(closure, |_| ())
    }

    /// Like [`handle`], but also accepts a callback to be executed after the request has been handled.
    ///
    /// [`handle`]: (Dispatcher::handle)
    pub fn handle_callback<R, F>(&mut self, handler: fn(&mut S, R::Params) -> R::Result, callback: F) -> &mut Self
    where
        F: FnOnce(&R::Result),
        R: lsp_types::request::Request,
    {
        let Some(ref request) = self.request else { return self; };

        if request.method != R::METHOD {
            return self;
        }

        let mut request = self.request.take().unwrap_or_else(|| unreachable!("checked as Some above"));

        let params = serde_json::from_value::<R::Params>(request.params.take())
            .unwrap_or_else(|_| unreachable!("must be run only once, as request.take() is performed above"));

        let result = handler(self.backend, params);

        callback(&result);

        let msg = Message::Response(Response {
            id: request.id,
            result: Some(serde_json::to_value(result).unwrap_or_else(|_| unreachable!("lsp_types crate is assumed to be correct"))),
            error: None,
        });

        self.connection
            .send(msg)
            .unwrap_or_else(|err| error!("Failed to send message to writer thread: {err}"));

        self
    }

    /// Fallthrough handler if the [`Request`] is not handled by any [`handle`]-like methods.
    ///
    /// This function should be used at the end of the [`handle`]-like methods chain.
    ///
    /// [`handle`]: (Dispatcher::handle)
    pub fn handle_fallthrough(&mut self, error_response: Error) {
        let Some(ref request) = self.request else { return; };

        warn!("{} on {}", error_response.message, request.method);

        let response = Message::Response(Response {
            id: request.id,
            result: None,
            error: Some(error_response),
        });

        self.connection
            .send(response)
            .unwrap_or_else(|err| error!("Failed to send message to writer thread: {err}"));
    }
}
