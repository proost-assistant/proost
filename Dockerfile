ARG	RUST_VERSION=1.63.0

FROM	rust:${RUST_VERSION}-alpine

RUN     apk add --no-cache openssh-client

RUN	rustup toolchain install ${RUST_VERSION} --component clippy rustfmt rust-docs
RUN	rustup default ${RUST_VERSION}
