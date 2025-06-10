use std::sync::{Arc};
use tokio::sync::{RwLock, oneshot};
use serde::{Deserialize, Serialize};

use crate::meta::*;

pub struct Nic {
    pub uuid: Uuid,
    pub nicE: Arc<RwLock<NicE>>,
}

impl Nic {
    pub async fn send(&mut self, data: &impl Serialize) {
        println!("sendaf");
        self.nicE.write().await.send(self, data).await;
        println!("sendafss");
    }

    pub async fn recv(&mut self) -> () {
        self.nicE.clone().write().await.recv(self).await
    }
}

pub struct NicE {
    metaE: Arc<RwLock<MetaE>>,
}

impl NicE {
    pub fn new(metaE : Arc<RwLock<MetaE>>) -> Self {
        NicE { metaE }
    }

    pub async fn create(nicE: Arc<RwLock<NicE>>) -> Nic {
        Nic { uuid: nicE.read().await.metaE.write().await.uuid(), nicE: nicE.clone() }
    }

    pub async fn send(&mut self, nic: &Nic, data: &impl Serialize) {
        println!("sendf");
        self.metaE.write().await.send(nic.uuid, data)
    }

    pub async fn recv(&mut self, nic: &Nic) {
        let data = MetaE::recv(self.metaE.clone(), nic.uuid).await;
    }
}