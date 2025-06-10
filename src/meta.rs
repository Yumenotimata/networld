use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll, Waker};
use std::sync::{Arc};

use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use tokio::sync::{RwLock, oneshot};

pub type Uuid = u64;

pub struct Response {
    uuid: Uuid,
    metaE: Arc<RwLock<MetaE>>
}

pub struct MetaE {
    uuid: Uuid,
    pub connections: HashMap<Uuid, Uuid>,
    pub buffer: HashMap<Uuid, ()>,
    pub senders: HashMap<Uuid, oneshot::Sender<()>>
}

impl MetaE {
    pub fn new() -> Self {
        MetaE { 
            uuid: 0, 
            connections: HashMap::new(), 
            buffer: HashMap::new(),
            senders: HashMap::new()
        }
    }

    pub fn uuid(&mut self) -> Uuid {
        let uuid = self.uuid;
        self.uuid += 1;
        uuid
    }

    pub fn connect(&mut self, p1: Uuid, p2: Uuid) {
        self.connections.insert(p1, p2);
    }

    pub fn send(&mut self, src: Uuid, data: &impl Serialize) {                println!("f");
        let targets: Vec<Uuid> = self.connections.iter().filter_map(|conn| if *conn.0 == src { Some(*conn.1) } else { None }).collect();                println!("f");
        targets.iter().for_each(|dst| {
            self.buffer.insert(*dst, ());                println!("f");
            if let Some(sender) = self.senders.remove(dst) {
                println!("f");
                sender.send(());
            }
        });

    }

    pub async fn recv(metaE: Arc<RwLock<Self>>, src: Uuid) -> () {
        {
            let meta = metaE.read().await;
            if let Some(res) = meta.buffer.get(&src) {
                return *res;
            }
        }

        let (tx, rx) = oneshot::channel();
        {
            let mut meta = metaE.write().await;
            meta.senders.insert(src, tx);
        }

        let _ = rx.await;
    }
}