use std::net::SocketAddr;

use http_body_util::Full;
use http_body_util::{combinators::BoxBody, BodyExt, Empty};
use hyper::body::Bytes;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Method, Request, Response, StatusCode};
use hyper_util::rt::TokioIo;
use tokio::fs::File;
use tokio::io::AsyncReadExt;
use tokio::net::TcpListener;


use conqiler::compile::Compiler;


/// This is our service handler. It receives a Request, routes on its
/// path, and returns a Future of a Response.
async fn echo(
    req: Request<hyper::body::Incoming>,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
    match (req.method(), req.uri().path()) {
        // Simply echo the body back to the client.
        (&Method::POST, "/api/echo") => Ok(Response::new(req.into_body().boxed())),

        (&Method::POST, "/api/sample") => {
            let mut f = File::open("test-server/static/wat.wasm").await.unwrap();
            if f.metadata().await.unwrap().len() > 1024 * 16 {
                return Ok(Response::builder()
                    .status(StatusCode::PAYLOAD_TOO_LARGE)
                    .body(full("File too large"))
                    .unwrap());
            }
            let mut buf = Vec::new();
            f.read_to_end(&mut buf).await.unwrap();
            Ok(Response::builder()
                .status(StatusCode::OK)
                .body(full(buf))
                .unwrap())
        }

        (&Method::POST, "/api/compile") => {
            // TODO
            Ok(Response::builder()
                .status(StatusCode::NOT_IMPLEMENTED)
                .body(full("Not implemented"))
                .unwrap())
        }

        // Return the 404 Not Found for other routes.
        _ => {
            let mut not_found = Response::new(empty());
            *not_found.status_mut() = StatusCode::NOT_FOUND;
            Ok(not_found)
        }
    }
}

fn empty() -> BoxBody<Bytes, hyper::Error> {
    Empty::<Bytes>::new()
        .map_err(|never| match never {})
        .boxed()
}

fn full<T: Into<Bytes>>(chunk: T) -> BoxBody<Bytes, hyper::Error> {
    Full::new(chunk.into())
        .map_err(|never| match never {})
        .boxed()
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let addr = SocketAddr::from(([127, 0, 0, 1], 7878));

    let listener = TcpListener::bind(addr).await?;
    println!("Listening on http://{}", addr);
    loop {
        let (stream, _) = listener.accept().await?;
        let io = TokioIo::new(stream);

        tokio::task::spawn(async move {
            if let Err(err) = http1::Builder::new()
                .serve_connection(io, service_fn(echo))
                .await
            {
                println!("Error serving connection: {:?}", err);
            }
        });
    }
}
