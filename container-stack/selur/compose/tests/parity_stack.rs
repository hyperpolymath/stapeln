// SPDX-License-Identifier: PMPL-1.0-or-later
use assert_cmd::Command;
use axum::{
    extract::{Extension, Json, Path},
    http::StatusCode,
    routing::{get, post},
    Router, Server,
};
use serde_json::{json, Value};
use std::{
    net::SocketAddr,
    sync::Arc,
};
use tokio::{
    net::TcpListener,
    sync::{Mutex, oneshot},
};
use tower::ServiceBuilder;

#[tokio::test]
async fn parity_stack_acceptance() -> anyhow::Result<()> {
    let containers = Arc::new(Mutex::new(Vec::new()));
    let mcp_calls = Arc::new(Mutex::new(Vec::new()));

    let (svalinn_addr, svalinn_shutdown) =
        spawn_svalinn_server(containers.clone()).await?;
    let (vordr_addr, vordr_shutdown) = spawn_vordr_http_server(containers.clone()).await?;
    let (mcp_addr, mcp_shutdown) = spawn_vordr_mcp_server(mcp_calls.clone()).await?;

    let compose_file = "examples/parity/compose.toml";

    let run_env = |cmd: &mut Command| {
        cmd.env("SVALINN_URL", format!("http://{}", svalinn_addr));
        cmd.env("VORDR_MCP_URL", format!("http://{}", mcp_addr));
        cmd
    };

    run_env(
        &mut Command::cargo_bin("selur-compose")?
            .arg("-f")
            .arg(compose_file)
            .arg("up"),
    )
    .assert()
    .success();

    run_env(
        &mut Command::cargo_bin("selur-compose")?
            .arg("-f")
            .arg(compose_file)
            .arg("down")
            .arg("-v")
            .env("VORDR_URL", format!("http://{}", vordr_addr)),
    )
    .assert()
    .success();

    let calls = {
        let lock = mcp_calls.lock().await;
        lock.clone()
    };

    assert!(calls.contains(&"vordr_network_create".to_string()));
    assert!(calls.contains(&"vordr_volume_create".to_string()));
    assert!(calls.contains(&"vordr_network_rm".to_string()));
    assert!(calls.contains(&"vordr_volume_rm".to_string()));

    let _ = svalinn_shutdown.send(());
    let _ = vordr_shutdown.send(());
    let _ = mcp_shutdown.send(());

    Ok(())
}

async fn spawn_svalinn_server(
    containers: Arc<Mutex<Vec<String>>>,
) -> anyhow::Result<(SocketAddr, oneshot::Sender<()>)> {
    let app = Router::new()
        .route("/api/v2/run", post({
            let state = containers.clone();
            move |Json(payload): Json<Value>| {
                let state = state.clone();
                async move {
                    let service_name = payload
                        .get("name")
                        .and_then(|v| v.as_str())
                        .unwrap_or("service");
                    let container_id = format!("parity_{}", service_name);
                    {
                        let mut lock = state.lock().await;
                        lock.push(container_id.clone());
                    }
                    (
                        StatusCode::OK,
                        Json(json!({
                            "container_id": container_id,
                            "status": "running"
                        })),
                    )
                }
            }
        }))
        .route("/api/v1/containers/:id", get(|Path(_): Path<String>| async {
            (
                StatusCode::OK,
                Json(json!({
                    "status": "running"
                })),
            )
        }))
        .route("/api/v1/containers/:id/stop", post(|Path(_): Path<String>| async {
            StatusCode::OK
        }))
        .layer(ServiceBuilder::new().layer(Extension(containers.clone())));

    spawn_router(app).await
}

async fn spawn_vordr_http_server(
    containers: Arc<Mutex<Vec<String>>>,
) -> anyhow::Result<(SocketAddr, oneshot::Sender<()>)> {
    let app = Router::new()
        .route(
            "/api/v1/containers",
            get(move || {
                let containers = containers.clone();
                async move {
                    let list: Vec<Value> = {
                        let guard = containers.lock().await;
                        guard
                            .iter()
                            .map(|id| {
                                json!({
                                    "container_id": id,
                                    "state": "running"
                                })
                            })
                            .collect()
                    };
                    (StatusCode::OK, Json(Value::Array(list)))
                }
            }),
        )
        .layer(ServiceBuilder::new().layer(Extension(containers.clone())));

    spawn_router(app).await
}

async fn spawn_vordr_mcp_server(
    calls: Arc<Mutex<Vec<String>>>,
) -> anyhow::Result<(SocketAddr, oneshot::Sender<()>)> {
    let app = Router::new()
        .route("/", post({
            let calls = calls.clone();
            move |Json(payload): Json<Value>| {
                let calls = calls.clone();
                async move {
                    if let Some(name) = payload
                        .get("params")
                        .and_then(|params| params.get("name"))
                        .and_then(|v| v.as_str())
                    {
                        let mut lock = calls.lock().await;
                        lock.push(name.to_string());
                    }
                    let response = json!({
                        "jsonrpc": "2.0",
                        "id": payload.get("id").cloned().unwrap_or(Value::Null),
                        "result": {}
                    });
                    (StatusCode::OK, Json(response))
                }
            }
        }))
        .layer(ServiceBuilder::new().layer(Extension(calls.clone())));

    spawn_router(app).await
}

async fn spawn_router(
    app: Router,
) -> anyhow::Result<(SocketAddr, oneshot::Sender<()>)> {
    let listener = TcpListener::bind(("127.0.0.1", 0)).await?;
    let addr = listener.local_addr()?;
    let (shutdown_tx, shutdown_rx) = oneshot::channel();
    let server = axum::Server::from_tcp(listener)?
        .serve(app.into_make_service())
        .with_graceful_shutdown(async {
            let _ = shutdown_rx.await;
        });
    tokio::spawn(server);
    Ok((addr, shutdown_tx))
}
