use std::sync::{Arc};

use tokio::sync::{RwLock, oneshot};
use serde::{Deserialize, Serialize};
use std::time::Duration;
use networld::*;

#[derive(Serialize)]
pub struct Frame {

}

#[tokio::main]
pub async fn main() {
    let metaE = Arc::new(RwLock::new(MetaE::new()));
    let nicE = Arc::new(RwLock::new(NicE::new(metaE.clone())));

    let mut nic0 = NicE::create(nicE.clone()).await;
    let mut nic1 = NicE::create(nicE.clone()).await;
    {
        metaE.write().await.connect(nic0.uuid, nic1.uuid);
    }

    
    let task2 = tokio::spawn(async move {
        println!("send");
        nic0.send(&Frame {}).await;
    });

    let task1 = tokio::spawn(async move {
        println!("receive");
        nic1.recv().await;
        println!("received");
    });


    let _ = tokio::join!(task1, task2);
}
