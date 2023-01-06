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
/// Each [`handle`]-like method will look at the method of [`Request`]. If there is a match, the corresponding [`LanguageServer`]
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

    /// Dispatches the [`Request`] to the [`LanguageServer`], if the associated method corresponds to
    /// [`lsp_types::request::Request::METHOD`].
    #[allow(dead_code)]
    #[no_coverage]
    pub fn handle<R>(&mut self, closure: fn(&mut S, R::Params) -> R::Result) -> &mut Self
    where
        R: lsp_types::request::Request,
    {
        self.handle_callback::<R, _>(closure, |_| ())
    }

    /// Like [`handle`], but also accepts a callback to be executed after the request has been handled.
    ///
    /// [`handle`]: (Dispatcher::handle)
    #[no_coverage]
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
    #[no_coverage]
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

#[cfg(test)]
mod tests {
    use lsp_types::request::{Initialize, Shutdown};
    use lsp_types::{InitializeParams, InitializeResult};
    use mockall::predicate::eq;

    use super::*;
    use crate::lsp::message::response::ErrorCode;
    use crate::lsp::{connection, MockLanguageServer};

    fn mock_initialize(mock: &mut MockLanguageServer, params: InitializeParams) -> InitializeResult {
        mock.initialize(params)
    }

    fn mock_shutdown(mock: &mut MockLanguageServer, params: ()) {
        mock.shutdown(params);
    }

    #[test]
    fn dispatcher_not_handled() {
        let mut backend = MockLanguageServer::new();
        let mut connection = connection::MockServer::new();

        let error = Error {
            code: ErrorCode::MethodNotFound,
            message: "Method not found".to_owned(),
            data: None,
        };

        let message = Message::Response(Response {
            id: 0,
            result: None,
            error: Some(error.clone()),
        });

        connection.expect_send().with(eq(message)).return_const(Ok(()));

        Dispatcher::new(Request::new::<Initialize>(0, InitializeParams::default()), &mut backend, &connection)
            .handle_fallthrough(error);
    }

    #[test]
    fn dispatcher_handled() {
        let mut backend = MockLanguageServer::new();
        let mut connection = connection::MockServer::new();

        let error = Error {
            code: ErrorCode::MethodNotFound,
            message: "Method not found".to_owned(),
            data: None,
        };

        let request = Request::new::<Initialize>(0, InitializeParams::default());
        let response = Response::new::<Initialize>(0, InitializeResult::default());

        backend
            .expect_initialize()
            .with(eq(InitializeParams::default()))
            .times(2)
            .return_const(InitializeResult::default());

        connection
            .expect_send()
            .with(eq(Message::Response(response)))
            .times(2)
            .return_const(Ok(()));

        Dispatcher::new(request.clone(), &mut backend, &connection)
            .handle::<Initialize>(mock_initialize)
            .handle::<Shutdown>(mock_shutdown)
            .handle_fallthrough(error.clone());

        Dispatcher::new(request, &mut backend, &connection)
            .handle::<Shutdown>(mock_shutdown)
            .handle::<Initialize>(mock_initialize)
            .handle_fallthrough(error);
    }
}
